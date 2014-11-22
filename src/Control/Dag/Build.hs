{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


-- deals with stiching together all the types in the graph.


-- this module badly requires further abstraction:
--   input collectors (run parent?, get output)
--   job runner
--   output committers (already know path and message)
--
--   don't use GitNode, use another. Maybe go back and fetch an old Node class
--   for a simple push dag. Cont could be fun here.


-- Let this grow for a while? It's clear there is alot of logic in here and we leave the complexity
-- bottleneck be for a while to get an idea how to refactor it.


module Control.Dag.Build where

import           Control.Applicative
import           Control.Monad
import           Control.Lens
import qualified Data.Conduit.List as CL

import           Control.Dag.Algorithm
import           Control.Dag.Backends.GitCmd
import           Control.Dag.Commit
import           Control.Dag.Prelude
import           Control.Dag.Types
import           Control.Dag.Utils


-- the correct place to deal with reading the inputs is in the algorithm.
-- so if the inputs are a trio of conduits, the algorithm will have to deal
-- with reading and parsing them. It would however be nice if we could
-- stream everything as Many's. So we can. The problem is that Many is not
-- an *exclusive* container as it should be. So get rid of ManyOf,
-- and create a terminator function.


execute :: GitNode m o -> m (InputHeader, Source m o)
execute = gRunner_


-- | Remove type information from this node. It then serves as a
terminate :: Monad m => GitNode m a -> GitNode m ()
terminate n = n { gRunner_ = liftM (set _2 ()) (gRunner_ n) }


type Suffix             = FilePath
type Suffix2            = (FilePath, FilePath)
type Suffix3            = (FilePath, FilePath, FilePath)
type GitNode2 m a b     = (GitNode m a, GitNode m b)
type GitNode3 m a b c   = (GitNode m a, GitNode m b, GitNode m c)
type Source2 m a b      = (Source m a, Source m b)
type Source3 m a b c    = (Source m a, Source m b, Source m c)
type Committer o m      = InputVersions -> o -> m () -- committers are sinks, therefore output must be piped into them
type Runner m o         = Committer o m -> m ()      -- multiple outputs must be piped into multiple committers.
type ClosedInputs m     = [GitNode m ()]             -- for this reason, the commit callback is a function that yields multiple committers and and not a pipe.


run0in :: (App m, Monad m, Read o, Show o)
          => FilePath
          -> Algorithm (Source m o) m
          -> (ClosedInputs m , Runner m o)
run0in path (Algorithm pipe getAlgoVer) = (,) [] $ \commit -> do
    algoVer <- getAlgoVer
    let newVers = InputVersions algoVer []
        effect = commit newVers pipe
    gitCheckRunEffect path newVers effect


-- closers return the same input nodes but with their runners swapped for errors.


run1in :: (App m, Monad m, Read a, Read o, Show a, Show o)
       => FilePath
       -> GitNode m a
       -> Algorithm (Source m a -> m o) m
       -> (ClosedInputs m , Runner m o)
run1in path input (Algorithm f getAlgoVer) = ([terminate input], runner)
  where
    runner commit = do
        algoVer <- getAlgoVer
        (head', pipe) <- gRunner_ input
        let newVers = InputVersions algoVer [head']
            effect = f pipe >>= commit newVers
        gitCheckRunEffect path newVers effect


run2in :: (App m, Monad m, Read a, Read b, Show a, Show b)
          => FilePath
          -> GitNode2 m a b
          -> Algorithm (Source2 m a b -> m o) m
          -> (ClosedInputs m , Runner m o)
run2in path (input1, input2) (Algorithm f getAlgoVer) = (inputs, runner)
  where
    runner commit = do
        algoVer <- getAlgoVer
        (head1, pipe1) <- gRunner_ input1
        (head2, pipe2) <- gRunner_ input2
        let newVers = InputVersions algoVer [head1, head2]
            effect = f (pipe1, pipe2) >>= commit newVers
        gitCheckRunEffect path newVers effect
    inputs = [terminate input1, terminate input2]


run3in :: (App m, Monad m, Read a, Read b, Read c, Show a, Show b, Show c)
          => FilePath
          -> GitNode3 m a b c
          -> Algorithm (Source3 m a b c -> m o) m
          -> (ClosedInputs m , Runner m o)
run3in path (input1, input2, input3) (Algorithm f getAlgoVer) = (inputs, runner)
  where
    runner commit = do
        algoVer <- getAlgoVer
        (head1, pipe1) <- gRunner_ input1
        (head2, pipe2) <- gRunner_ input2
        (head3, pipe3) <- gRunner_ input3
        let newVers = InputVersions algoVer [head1, head2, head3]
            effect = f (pipe1, pipe2, pipe3) >>= commit newVers
        gitCheckRunEffect path newVers effect
    inputs = [terminate input1, terminate input2, terminate input3]


run1out :: (App m, Monad m, Read a, Show a)
        => FilePath
        -> Suffix
        -> (ClosedInputs m , Runner m (Source m a))
        -> GitNode m a
run1out basePath suf (inputs, run) = GitNode path inputs effect
  where
    path = fixPaths [basePath, suf]
    commit inputVers pipe = pipe $$ showCommitSink path inputVers
    effect = run commit >> gitReadOutput path


run2out :: (App m, Monad m, Read a, Read b, Show a, Show b)
        => FilePath
        -> Suffix2
        -> (ClosedInputs m , Runner m (Source2 m a b))
        -> GitNode2 m a b
run2out basePath suffixes (inputs, run) = (gn path1, gn path2)
  where
    gn path = GitNode path inputs $ run commit >> gitReadOutput path
    (path1, path2) = over each (\s -> fixPaths [basePath, s]) suffixes
    commit inputVers (p1, p2) = do
         p1 $$ showCommitSink path1 inputVers
         p2 $$ showCommitSink path2 inputVers


run3out :: (App m, Monad m, Read a, Read b, Read c, Show a, Show b, Show c)
        => FilePath
        -> Suffix3
        -> (ClosedInputs m , Runner m (Source3 m a b c))
        -> GitNode3 m a b c
run3out basePath suffixes (inputs, run) = (gn path1, gn path2, gn path3)
  where
    gn path = GitNode path inputs $ run commit >> gitReadOutput path
    (path1, path2, path3) = over each (\s -> fixPaths [basePath, s]) suffixes
    commit inputVers (p1, p2, p3) = do
         p1 $$ showCommitSink path1 inputVers
         p2 $$ showCommitSink path2 inputVers
         p3 $$ showCommitSink path3 inputVers


in0out1 :: (App m, Read a, Show a)
        => FilePath
        -> Suffix
        -> Algorithm (Source m (Source m a)) m
        -> GitNode m a
in0out1 path suf  = run1out path suf  . run0in path
in0out2 path sufs = run2out path sufs . run0in path
in0out3 path sufs = run3out path sufs . run0in path


in1out1 :: (App m, Read a, Read b, Show a, Show b)
        => FilePath
        -> Suffix
        -> GitNode m a
        -> Algorithm (Source m a -> Source m b) m
        -> GitNode m b
in1out1 path suf  input  = run1out path suf  . run1in path input
in1out2 path sufs input  = run2out path sufs . run1in path input
in1out3 path sufs input  = run3out path sufs . run1in path input


in2out2 :: (App m, Read a, Read b, Read c, Read d, Show a, Show b, Show c, Show d)
        => FilePath
        -> Suffix2
        -> GitNode2 m a b
        -> Algorithm (Source2 m a b -> Source2 m c d) m
        -> GitNode2 m c d
in2out2 path sufs inputs = run2out path sufs . run2in path inputs
in2out1 path suf  inputs = run1out path suf  . run2in path inputs
in2out3 path sufs inputs = run3out path sufs . run2in path inputs


in3out3 :: (App m, Read a, Read b, Read c, Read d, Read e, Read f, Show a, Show b, Show c, Show d, Show e, Show f)
        => FilePath
        -> Suffix3
        -> GitNode3 m a b c
        -> Algorithm (Source3 m a b c -> Source3 m d e f) m
        -> GitNode3 m d e f
in3out3 path sufs inputs = run3out path sufs . run3in path inputs
in3out1 path suf  inputs = run1out path suf  . run3in path inputs
in3out2 path sufs inputs = run2out path sufs . run3in path inputs


gitCheckRunEffect :: App m => FilePath -> InputVersions -> m () -> m ()
gitCheckRunEffect path inputVers effect = do
    info "Called"
    exists <- gitExists path
    go <- if exists
        then do changed <- compareVers inputVers
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
    compareVers new = do
        old <- read <$> gitMessage path
        return $ old /= new


-- | This ugly brute should probably not exist... However everything needs
--   serialization. It's not clear if / when a better solution will be
--   required (rather than Show, that is).
showCommitSink :: (App m, Show a) => FilePath -> InputVersions -> Sink a m ()
showCommitSink path ivs = CL.map (pack . show) =$ commitSink path ivs
