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


import           Control.Applicative
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


run0in path (Algo f v) commit = let newMsg = gitCommitMessage path v []
                                    effect = f >>= commit newMsg
                                in gitCheckRunEffect path newMsg effect



type Committer o m= String -> o -> m ()

type Pair'O'Suffixes = (FilePath, FilePath)

type Pair'O'GitNodes m a b = (GitNode m a, GitNode m b)


run2in :: (HasGit m, Monad m, Read a, Read b, Show a, Show b)
          => FilePath -> Pair'O'GitNodes m a b
          -> Algo ((a, b) -> m o) -> Committer o m
          -> m ()
run2in path (input1, input2) (Algo f v) commit = do
    (head1, body1) <- gRunner_ input1
    (head2, body2) <- gRunner_ input2
    let newMsg = gitCommitMessage path v [head1, head2]
        effect = f (body1, body2) >>= commit newMsg
    gitCheckRunEffect path newMsg effect


run2out :: (HasGit m, Monad m, Read a, Read b, Show a, Show b)
        => FilePath -> Pair'O'Suffixes
        -> (Committer (a, b) m -> m ()) -> Pair'O'GitNodes m a b
run2out basePath (suf1, suf2) run =
    ( mkn path1 (run commit >> gitReadOutput path2)
    , mkn path2 (run commit >> gitReadOutput path2)
    )
  where
    mkn p = GitNode p [error "input paths todo"]
    (path1, path2) = (fixPaths [basePath, suf1], fixPaths [basePath, suf2])
    commit msg (a, b) = let c p = gitCommit p msg . show
                        in (c path1 a >> c path2 b)


in2out2 :: (HasGit m, Monad m, Read a, Read b, Read c, Read d, Show a, Show b, Show c, Show d)
        => FilePath
        -> Pair'O'Suffixes
        -> Pair'O'GitNodes m a b
        -> Algo ((a, b) -> m (c, d))
        -> Pair'O'GitNodes m c d
in2out2 path sufs inputs = run2out path sufs . run2in path inputs







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
