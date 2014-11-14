{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Node.JunctionNode
    ( JunctionNode(..)
    ) where


import Control.Monad
import Data.Many
import Data.Monoid
import Control.Dag.Types.Node


-- | Collects inputs and produces outputs if complete and changed
data JunctionNode i n = JunctionNode i n


instance (Functor m, Monad m, Eq i, Many i, Monoid i, Node i n m) =>
    Node i (JunctionNode i n) m
  where
    fold (JunctionNode state output) change = do
        when emit $ send output state'
        return $ JunctionNode state' output
      where
        state' = change <> state
        emit = isComplete state' && state' /= state
