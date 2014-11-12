{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RankNTypes
  , TypeSynonymInstances
  #-}

module Control.Purity.Internal.Dag
    ( Dag
    , DagState
    , NodeId
    , Node(..)
    , SimpleNode(..)
    , addNode
    , emptyState
    , send
    , runDag
    ) where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Dynamic
import           Data.IntMap.Strict as IntMap


-- Nodes have versioned output (GIT / AcidState)
-- Node runner backend for farming out work


newtype NodeId n a m = NodeId Int
    deriving Typeable


type DagState = IntMap.IntMap Dynamic


newtype Dag m a = Dag (StateT DagState m a)
    deriving (Applicative, Functor, Monad,
              MonadIO, MonadTrans, Typeable, MonadState DagState)


class ( Functor m, Monad m
      , Typeable n, Typeable a, Typeable m
      ) => Node n a m
  where
      send' :: Node n a m => n -> a -> Dag m n


newtype SimpleNode f a m = SimpleNode (a -> Dag m ())
    deriving (Typeable)


instance (Functor m, Monad m, Typeable a, Typeable f, Typeable m) =>
         Node (SimpleNode f a m) a m
  where
    send' (SimpleNode f) a = f a >> pure (SimpleNode f)


emptyState :: DagState
emptyState = IntMap.empty


runDag :: Monad m => Dag m a -> m a
runDag (Dag m) = evalStateT m IntMap.empty


addNode :: Node n a m => n -> Dag m (NodeId n a (m ()))
addNode node = do
    map' <- get
    let i = IntMap.size map'
    let dyn = toDyn node
    put $ IntMap.insert i dyn map'
    return $ NodeId i


getNode :: Node n a m => NodeId n a (m r) -> Dag m n
getNode (NodeId nid) = do
    node <- fmap (!nid) get
    return $ fromDyn node $ error "ghosts"


putNode :: Node n a m => NodeId n a (m r) -> n -> Dag m ()
putNode (NodeId nid) = modify . IntMap.insert nid . toDyn


send :: Node n a m => NodeId n a (m r) -> a -> Dag m ()
send nid v = do
    node <- getNode nid
    node' <- send' node v
    putNode nid node'
