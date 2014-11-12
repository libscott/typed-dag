{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Node.StateNode
    ( DagState
    , StateNode
    , StateNodeT
    , TypeableNode
    , emptyDag
    , addNode
    , runStateNodeT
    , execStateNodeT
    , evalStateNodeT
    ) where


import           Control.Monad.State
import           Data.Dynamic
import qualified Data.IntMap.Strict as IntMap

import           Control.Dag.Types.Node


type DagState = IntMap.IntMap Dynamic


type StateNodeT = StateT DagState


newtype StateNode n a m = StateNode Int


class (Typeable n, Typeable a, Typeable m, Node n a m) => TypeableNode n a m


instance (MonadState DagState m, TypeableNode n a m) =>
    Node (StateNode n a (t m)) a m
  where
    send nid a = do
        node <- getNode nid
        node' <- fold node a
        putNode nid node'


emptyDag :: DagState
emptyDag = IntMap.empty


addNode :: (MonadState DagState m, Typeable n) => n -> m (StateNode n a (t m))
addNode node = do
    map' <- get
    let i = IntMap.size map'
    let dyn = toDyn node
    put $ IntMap.insert i dyn map'
    return $ StateNode i


runStateNodeT :: StateNodeT m a -> DagState -> m (a, DagState)
runStateNodeT = runStateT


execStateNodeT :: Monad m => StateNodeT m a -> DagState -> m DagState
execStateNodeT = execStateT


evalStateNodeT :: Monad m => StateNodeT m a -> DagState -> m a
evalStateNodeT = evalStateT


getNode :: (MonadState DagState m, Node n a m, Typeable n) => StateNode n a (t m) -> m n
getNode (StateNode nid) = do
    node <- fmap (IntMap.! nid) get
    return $ fromDyn node $ error "DagState mixup"


putNode :: (MonadState DagState m, TypeableNode n a m, Typeable n) => StateNode n a (t m) -> n -> m ()
putNode (StateNode nid) node = modify . IntMap.insert nid $ toDyn node
