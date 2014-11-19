{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE KindSignatures             #-}


-- this module badly requires further abstraction:
--   input collectors (run parent?, get output)
--   job runner
--   output committers (already know path and message)
--
--   don't use GitNode, use another. Maybe go back and fetch an old Node class
--   for a simple push dag. Cont could be fun here.


module Control.Dag
    ( module Control.Dag.Utils
    , Algo (..)
    , AlgoVersion (..)
    , GitNode (..)
    , HasGit
    , Pair'O'Suffixes
    , Pair'O'GitNodes
    , Trio'O'Suffixes
    , Trio'O'GitNodes
    , codeHash
    , execute
    , in0out1
    , in0out2
    , in1out1
    , in1out2
    , in2out1
    , in2out2
    , in3out1
    , in3out2
    , in3out3
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


type Committer o m = String -> o -> m ()
type Pair'O'Suffixes = (FilePath, FilePath)
type Pair'O'GitNodes m a b = (GitNode m a, GitNode m b)
type Trio'O'Suffixes = (FilePath, FilePath, FilePath)
type Trio'O'GitNodes m a b c = (GitNode m a, GitNode m b, GitNode m c)


run0in :: (HasGit m, Monad m, Read o, Show o)
          => FilePath
          -> Algo (m o)
          -> Committer o m
          -> m ()
run0in path (Algo f v) commit = gitCheckRunEffect path newMsg effect
  where newMsg = gitCommitMessage path v []
        effect = f >>= commit newMsg


run1in :: (HasGit m, Monad m, Read i, Read o, Show i, Show o)
       => FilePath
       -> GitNode m i
       -> Algo (i -> m o)
       -> Committer o m
       -> m ()
run1in path input (Algo f v) commit = do
    (head', body) <- gRunner_ input
    let newMsg = gitCommitMessage path v [head']
        effect = f body >>= commit newMsg
    gitCheckRunEffect path newMsg effect


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


run3in :: (HasGit m, Monad m, Read a, Read b, Read c, Show a, Show b, Show c)
          => FilePath -> Trio'O'GitNodes m a b c
          -> Algo ((a, b, c) -> m o) -> Committer o m
          -> m ()
run3in path (input1, input2, input3) (Algo f v) commit = do
    (head1, body1) <- gRunner_ input1
    (head2, body2) <- gRunner_ input2
    (head3, body3) <- gRunner_ input3
    let newMsg = gitCommitMessage path v [head1, head2, head3]
        effect = f (body1, body2, body3) >>= commit newMsg
    gitCheckRunEffect path newMsg effect


run1out :: (HasGit m, Monad m, Read o, Show o)
        => FilePath
        -> (Committer o m -> m ())
        -> GitNode m o
run1out path run = node $ run commit >> gitReadOutput path
  where
    node = GitNode path ["input paths todo"]
    commit msg = gitCommit path msg . show


run2out :: (HasGit m, Monad m, Read a, Read b, Show a, Show b)
        => FilePath -> Pair'O'Suffixes
        -> (Committer (a, b) m -> m ())
        -> Pair'O'GitNodes m a b
run2out basePath suffixes run =
    ( mkn path1 (run commit >> gitReadOutput path1)
    , mkn path2 (run commit >> gitReadOutput path2)
    )
  where
    mkn p = GitNode p ["input paths todo"]
    (path1, path2) = over each (\s -> fixPaths [basePath, s]) suffixes
    commit msg (a, b) = let c p = gitCommit p msg . show
                        in (c path1 a >> c path2 b)


run3out :: (HasGit m, Monad m, Read a, Read b, Read c, Show a, Show b, Show c)
        => FilePath
        -> Trio'O'Suffixes
        -> (Committer (a, b, c) m -> m ())
        -> Trio'O'GitNodes m a b c
run3out basePath suffixes run =
    ( mkn path1 (run commit >> gitReadOutput path1)
    , mkn path2 (run commit >> gitReadOutput path2)
    , mkn path3 (run commit >> gitReadOutput path3)
    )
  where
    mkn p = GitNode p ["input paths todo"]
    (path1, path2, path3) = over each (\s -> fixPaths [basePath, s]) suffixes
    commit msg (a, b, c) = let comit p = gitCommit p msg . show
                           in comit path1 a >> comit path2 b >> comit path3 c


in0out1 :: (HasGit m, Monad m, Read a, Show a)
        => FilePath
        -> Algo (m a)
        -> GitNode m a
in0out1 path = run1out path . run0in path
in0out2 path sufs = run2out path sufs . run0in path
in0out3 path sufs = run3out path sufs . run0in path


in1out1 :: (HasGit m, Monad m, Read a, Read b, Show a, Show b)
        => FilePath
        -> GitNode m a
        -> Algo (a -> m b)
        -> GitNode m b
in1out1 path      input  = run1out path      . run1in path input
in1out2 path sufs input  = run2out path sufs . run1in path input
in1out3 path sufs input  = run3out path sufs . run1in path input


in2out2 :: (HasGit m, Monad m, Read a, Read b, Read c, Read d, Show a, Show b, Show c, Show d)
        => FilePath
        -> Pair'O'Suffixes
        -> Pair'O'GitNodes m a b
        -> Algo ((a, b) -> m (c, d))
        -> Pair'O'GitNodes m c d
in2out2 path sufs inputs = run2out path sufs . run2in path inputs
in2out1 path      inputs = run1out path      . run2in path inputs
in2out3 path sufs inputs = run3out path sufs . run2in path inputs


in3out3 :: (HasGit m, Monad m, Read a, Read b, Read c, Read d, Read e, Read f, Show a, Show b, Show c, Show d, Show e, Show f)
        => FilePath
        -> Trio'O'Suffixes
        -> Trio'O'GitNodes m a b c
        -> Algo ((a, b, c) -> m (d, e, f))
        -> Trio'O'GitNodes m d e f
in3out3 path sufs inputs = run3out path sufs . run3in path inputs
in3out1 path      inputs = run1out path      . run3in path inputs
in3out2 path sufs inputs = run2out path sufs . run3in path inputs


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


gitCommitMessage :: FilePath -> AlgoVersion -> [GitHeader] -> String
gitCommitMessage path ver heads = show (path, ver, sort heads)
