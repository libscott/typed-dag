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
    , addNode
    , emptyState
    , send
    , runDag
    ) where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Dynamic
import           Data.IntMap.Strict as IntMap


newtype NodeId n a m = NodeId Int


type DagState = IntMap.IntMap Dynamic


newtype Dag m a = Dag (StateT DagState m a)
    deriving (Applicative, Functor, Monad,
              MonadIO, MonadState DagState,
              MonadTrans, Typeable)


class (Functor m, Monad m, Typeable n, Typeable a, Typeable m) =>
      Node n a m
  where
    send' :: (Monad m, Node n a m) => n -> a -> Dag m n


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


getNode :: Node n a m => NodeId n a (m ()) -> Dag m n
getNode (NodeId nid) = do
    node <- fmap (!nid) get
    return $ fromDyn node (error "you have reached a secret level where the impossible is possible")


putNode :: (Functor m, Monad m, Typeable n) => NodeId n a (m ()) -> n -> Dag m ()
putNode (NodeId nid) node = modify $ IntMap.insert nid (toDyn node)


send :: Node n a m => NodeId n a (m ()) -> a -> Dag m ()
send nid v = do
    node <- getNode nid
    node' <- send' node v
    putNode nid node'
