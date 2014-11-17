{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag
    ( demo
    ) where


import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Many
import           System.IO

-- import           Control.Dag.Node.EmitterNode
-- import           Control.Dag.Node.GitNode
-- import           Control.Dag.Node.JobNode
-- import           Control.Dag.Test.Inputs
-- import           Control.Dag.Types


type Dag s m = StateT (s m) m



-- | Input nodes
node0 :: n -> (n, ())
node0 n = (n, ())
node1 :: n -> a -> (n, a)
node1 n a = (n, a)
node2 :: n -> a -> b -> (n, (a, b))
node2 n a b = (n, (a, b))


buildDag :: n -> Int
buildDag = undefined




data Pull node upstreams = Pull node upstreams


-- the magic is that the typeclass should be able to figure out
-- automatically how to return the outputs tuple based on the instances.
-- the output tuples are single output Pull nodes which call the parent with a
-- set of callbacks.
--
-- The structure itself is pure, even to create.
-- The GIT pull node is an additional typeclass which allows the structure to
-- be walked and nodes indexed by their name.
-- Perhaps it would even be possible for the walk function to be genericized.

-- o: output callbacks
-- u: upstreams
class (Functor m, Monad m) => PullNode n o (m :: * -> *)
  where
      execute :: n -> o -> m ()
      pull :: n -> o



instance PullNode (Pull n (a, b) m) (a, b) m where
    -- return a set of output proxies corresponding to (a, b)
    pull n upstreams = ()



data OutputSelector a = OutputSelector a




dag :: Dag MySubscribers IO ()
dag = do
    -- dag builder outputs nodes wrapped in address container.
    let jobNode = Pull InputNode ()
        (gn1, gn2) = Pull (GitNode "abc" jobNode) ()
        outputNode = pull1 PrinterNode gitNode
    runDag outputNode



data InputNode = InputNode
instance PullNode (InputNode o) o m where
    -- return a tuple of nodes which can be called to produce output.
    pull node upstreams =



data MyJobNode o = MyJobNode o
instance PullNode (MyJobNode o) o m where
    -- return a tuple of nodes which can be called to produce output.
    pull node = (OutputNode node, OutputNode node)


data PrinterNode i = PrinterNode
instance (Functor m, MonadIO m, Show i) => Node i (PrinterNode i) m where
    send _ = liftIO . print
