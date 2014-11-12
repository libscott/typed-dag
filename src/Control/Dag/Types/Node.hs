{-# LANGUAGE MultiParamTypeClasses      #-}


module Control.Dag.Types.Node
    ( Node(..)
    ) where

import Control.Monad


class (Functor m, Monad m) => Node n a m
  where
      send :: Node n a m => n -> a -> m ()
      send n a = void $ fold n a
      fold :: Node n a m => n -> a -> m n
      fold n a = send n a >> return n
