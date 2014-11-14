{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Node.StateNode
    ( DagState
    , DagStateT
    , StateNode
    , addStateNode
    , emptyDag
    , runNodeState
    , execNodeState
    , evalNodeState
    ) where


import           Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap

import           Control.Dag.Types.Node


type DagState n = IntMap.IntMap n


type DagStateT n = StateT (DagState n)


newtype StateNode i n = StateNode Int


instance ( Functor m
         , MonadState (DagState n) m
         , Node i n m
         )
        => Node i (StateNode i n) m
  where
    send nid input = do
        node <- getNode nid
        node' <- fold node input
        putNode nid node'


emptyDag :: DagState n
emptyDag = IntMap.empty


addStateNode :: (MonadState (DagState n) m, Node i n m) => n -> m (StateNode i n)
addStateNode node = do
    map' <- get
    let i = IntMap.size map'
    put $ IntMap.insert i node map'
    return $ StateNode i


runNodeState :: DagStateT n m a -> DagState n -> m (a, DagState n)
runNodeState = runStateT


execNodeState :: Monad m => DagStateT n m a -> DagState n -> m (DagState n)
execNodeState = execStateT


evalNodeState :: Monad m => DagStateT n m a -> DagState n -> m a
evalNodeState = evalStateT


getNode :: (MonadState (DagState n) m, Node i n m) => StateNode i n -> m n
getNode (StateNode nid) = fmap (IntMap.! nid) get


putNode :: (MonadState (DagState n) m) => StateNode i n -> n -> m ()
putNode (StateNode nid) = modify . IntMap.insert nid
