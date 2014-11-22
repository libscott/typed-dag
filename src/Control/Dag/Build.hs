module Control.Dag.Build where

import           Control.Applicative
import           Control.Lens
import qualified Data.Conduit.List as CL

import           Control.Dag.Algorithm
import           Control.Dag.Commit
import           Control.Dag.Prelude
import           Control.Dag.Runners
import           Control.Dag.Types
import           Control.Dag.Utils


execute :: GitNode m o -> m (InputHeader, Source m o)
execute = gRunner_


-- | todo: moar lenses no?
terminate :: App m => GitNode m a -> GitNode m ()
terminate n = n { gRunner_ = set _2 (return ()) <$> gRunner_ n }


type Suffix             = FilePath
type Suffix2            = (FilePath, FilePath)
type Suffix3            = (FilePath, FilePath, FilePath)
type ClosedInputs m     = [GitNode m ()]


---------------------------------------------------------
-- | Input closers close over the types of their inputs
---------------------------------------------------------

close0 :: App m
       => FilePath
       -> Algorithm (() -> m o) m
       -> (ClosedInputs m, Runner m o)
close0 path algo =
    let transpose = return ([], ())
    in ([], runner path transpose algo)


close1 :: (App m, Read a, Show a)
       => FilePath
       -> GitNode m a
       -> Algorithm (Source m a -> m o) m
       -> (ClosedInputs m, Runner m o)
close1 path i1 algo =
    let transpose = (_1 %~ (:[])) <$> gRunner_ i1
    in ([terminate i1], runner path transpose algo)


close2 :: (App m, Read a, Read b, Show a, Show b)
          => FilePath
          -> GitNode2 m a b
          -> Algorithm (Source2 m a b -> m o) m
          -> (ClosedInputs m, Runner m o)
close2 path (i1, i2) algo =
    let transpose = do
        (head1, pipe1) <- gRunner_ i1
        (head2, pipe2) <- gRunner_ i2
        return ([head1, head2], (pipe1, pipe2))
    in ([terminate i1, terminate i2], runner path transpose algo)


close3 :: (App m, Read a, Read b, Read c, Show a, Show b, Show c)
       => FilePath
       -> GitNode3 m a b c
       -> Algorithm (Source3 m a b c -> m o) m
       -> (ClosedInputs m, Runner m o)
close3 path (i1, i2, i3) algo =
    let transpose = do
        (head1, pipe1) <- gRunner_ i1
        (head2, pipe2) <- gRunner_ i2
        (head3, pipe3) <- gRunner_ i3
        return ([head1, head2, head3], (pipe1, pipe2, pipe3))
    in ( [terminate i1, terminate i2, terminate i3]
       , runner path transpose algo
       )


---------------------------------------------------------
-- | Node builders take closed inputs and return a node.
--   It is also the job of the node builder to provide
--   the commit function since it knows the output types.
---------------------------------------------------------


node1 :: (App m, Read a, Show a)
        => FilePath
        -> Suffix
        -> (ClosedInputs m, Runner m (Source m a))
        -> GitNode m a
node1 basePath suf (inputs, run) =
    GitNode path inputs $ run commit >> sourceOutput path
  where
    path = fixPaths [basePath, suf]
    commit inputVers pipe = pipe $$ showCommitSink path inputVers


node2 :: (App m, Read a, Read b, Show a, Show b)
        => FilePath
        -> Suffix2
        -> (ClosedInputs m, Runner m (Source2 m a b))
        -> GitNode2 m a b
node2 basePath suffixes (inputs, run) = (gn path1, gn path2)
  where
    gn path = GitNode path inputs $ run commit >> sourceOutput path
    (path1, path2) = over each (\s -> fixPaths [basePath, s]) suffixes
    commit inputVers (p1, p2) = do
        p1 $$ showCommitSink path1 inputVers
        p2 $$ showCommitSink path2 inputVers


node3 :: (App m, Read a, Read b, Read c, Show a, Show b, Show c)
        => FilePath
        -> Suffix3
        -> (ClosedInputs m, Runner m (Source3 m a b c))
        -> GitNode3 m a b c
node3 basePath suffixes (inputs, run) = (gn path1, gn path2, gn path3)
  where
    gn path = GitNode path inputs $ run commit >> sourceOutput path
    (path1, path2, path3) = over each (\s -> fixPaths [basePath, s]) suffixes
    commit inputVers (p1, p2, p3) = do
        p1 $$ showCommitSink path1 inputVers
        p2 $$ showCommitSink path2 inputVers
        p3 $$ showCommitSink path3 inputVers


---------------------------------------------------------
-- | O(n^2)
---------------------------------------------------------


in0out1 :: (App m, Read a, Show a)
        => FilePath
        -> Suffix
        -> Algorithm (() -> m (Source m a)) m
        -> GitNode m a
in0out1 path suf  = node1 path suf  . close0 path
in0out2 path sufs = node2 path sufs . close0 path
in0out3 path sufs = node3 path sufs . close0 path


in1out1 :: (App m, Show a, Read a, Show b, Read b)
        => FilePath
        -> Suffix
        -> GitNode m a
        -> Algorithm (Source m a -> m (Source m b)) m
        -> GitNode m b
in1out1 path suf  input  = node1 path suf  . close1 path input
in1out2 path sufs input  = node2 path sufs . close1 path input
in1out3 path sufs input  = node3 path sufs . close1 path input


in2out2 :: (App m, Read a, Read b, Read c, Read d, Show a, Show b, Show c, Show d)
        => FilePath
        -> Suffix2
        -> GitNode2 m a b
        -> Algorithm (Source2 m a b -> m (Source2 m c d)) m
        -> GitNode2 m c d
in2out2 path sufs inputs = node2 path sufs . close2 path inputs
in2out1 path suf  inputs = node1 path suf  . close2 path inputs
in2out3 path sufs inputs = node3 path sufs . close2 path inputs


in3out3 :: (App m, Read a, Read b, Read c, Read d, Read e, Read f, Show a, Show b, Show c, Show d, Show e, Show f)
        => FilePath
        -> Suffix3
        -> GitNode3 m a b c
        -> Algorithm (Source3 m a b c -> m (Source3 m d e f)) m
        -> GitNode3 m d e f
in3out3 path sufs inputs = node3 path sufs . close3 path inputs
in3out1 path suf  inputs = node1 path suf  . close3 path inputs
in3out2 path sufs inputs = node2 path sufs . close3 path inputs


-- | This ugly brute should probably not exist... However everything needs
--   serialization. It's not clear if / when a better solution will be
--   required (rather than Show, that is).
showCommitSink :: (App m, Show a) => FilePath -> InputVersions -> Sink a m ()
showCommitSink path ivs = CL.map (pack . show) =$ commitSink path ivs
