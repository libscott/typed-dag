{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE KindSignatures             #-}


module Control.Dag
    ( module Control.Dag.Utils
    , Algo (..)
    , AlgoVersion (..)
    , GitNode (..)
    , HasGit
    , codeHash
    , execute
    , gitNode0
    , gitNode1
    , gitNode2
    , split2
    , split3
    , withGit
    ) where


import           Control.Monad
import           Control.Lens
import           Data.List (sort)

import           Control.Dag.Algorithm
import           Control.Dag.Backends.GitCmd
import           Control.Dag.Utils


execute :: GitNode m o -> m (GitHeader, o)
execute = gRunner_


data GitNode (m :: * -> *) o = GitNode
    { gPath_   :: FilePath
    , gInputs_ :: [FilePath]
    , gRunner_ :: m (GitHeader, o)
    }


gitNode0 :: (HasGit m, Read o, Show o) => FilePath -> Algo (m o) -> (o -> m ()) -> GitNode m o
gitNode0 path (Algo f v) commit = GitNode path [] $ do
    let newMsg = gitCommitMessage path v []
    gitCheckRunEffect path newMsg (f >>= commit)
    gitReadOutput path


gitNode0_2 :: (HasGit m, Read a, Show a, Read b, Show b) => FilePath -> (FilePath, FilePath) -> Algo (m (a, b)) -> (GitNode m a, GitNode m b)
gitNode0_2 path (suf1, suf2) (Algo f v) = split2 (suf1, suf2) $ GitNode path [] $ do
    let newMsg = gitCommitMessage path v []
    gitCheckRunEffect path newMsg (f >>= commit2 newMsg)
    gitReadOutput path
  where
    commit2 msg (a, b) = do gitCommit (fixPaths [path, suf1]) msg $ show a
                            gitCommit (fixPaths [path, suf2]) msg $ show b


commit2 :: (HasGit m, Show a) => FilePath -> String -> a -> m ()
commit2 path msg = gitCommit path msg . show


gitNode1 :: (HasGit m, Read o, Show o) => FilePath
                                       -> GitNode m a
                                       -> Algo (a -> m o)
                                       -> GitNode m o
gitNode1 path input (Algo f v) = GitNode path inputs $ do
    (depHead, body) <- gRunner_ input
    let newMsg = gitCommitMessage path v [depHead]
    gitCheckRunEffect path newMsg (f body)
    gitReadOutput path
  where
    inputs = [gPath_ input]


gitNode2 :: (HasGit m, Read o, Show o) => FilePath
                                       -> GitNode m a
                                       -> GitNode m b
                                       -> Algo ((a, b) -> m o)
                                       -> GitNode m o
gitNode2 path input1 input2 (Algo f v) = GitNode path inputs $ do
    (depHead1, body1) <- gRunner_ input1
    (depHead2, body2) <- gRunner_ input2
    let newMsg = gitCommitMessage path v [depHead1, depHead2]
    gitCheckRunEffect path newMsg (f (body1, body2))
    gitReadOutput path
  where
    inputs = [gPath_ input1, gPath_ input2]


split2 :: (HasGit m, Read a, Show b, Read b, Show a)
       => (FilePath, FilePath) -> GitNode m (a, b) -> (GitNode m a, GitNode m b)
split2 (suf1, suf2) node =
        ( gitNode1 path1 node $ Algo (return . (^._1)) Unversioned
        , gitNode1 path2 node $ Algo (return . (^._2)) Unversioned
        )
  where
    joinPath suf = fixPaths [gPath_ node, suf]
    (path1, path2) = (joinPath suf1, joinPath suf2)


split3 :: (HasGit m, Read a, Show b, Read b, Show a, Read c, Show c)
       => FilePath -> FilePath -> FilePath
       -> GitNode m (a, b, c)
       -> (GitNode m a, GitNode m b, GitNode m c)
split3 suf1 suf2 suf3 node =
        ( gitNode1 path1 node $ Algo (return . (^._1)) Unversioned
        , gitNode1 path2 node $ Algo (return . (^._2)) Unversioned
        , gitNode1 path3 node $ Algo (return . (^._3)) Unversioned
        )
  where
    joinPath suf = fixPaths [gPath_ node, suf]
    (path1, path2, path3) = (joinPath suf1, joinPath suf2, joinPath suf3)


gitCheckRunEffect :: (HasGit m) => FilePath -> String -> m () -> m ()
gitCheckRunEffect path newMsg effect = do
    info "Called"
    exists <- gitExists path
    go <- if exists
        then do changed <- compareMessage newMsg
                if changed
                    then do info "Inputs changed"
                            return True
                    else do info "No change in inputs"
                            return False
        else do info "Creating"
                return True
    when go $ do
        info "Running job"
        effect
  where
    info s = info0 $ path ++ ": " ++ s
    compareMessage new = do
        old <- gitMessage path
        return $ old /= (new ++ "\n")


-- now need to change execution to write and commit outputs separately.
-- but, shit, we don't know the type here at all.
-- the answer is to pass the extraction function down the stack when assembling
-- the GitNode object. So gitNodeN_N.

executeEffect :: FilePath -> String -> m o -> m ()
executeEffect path commitMessage effect = undefined


gitCommitMessage :: FilePath -> AlgoVersion -> [GitHeader] -> String
gitCommitMessage path ver heads = show (path, ver, sort heads)
