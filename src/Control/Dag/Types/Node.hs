{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}


module Control.Dag.Types.Node
    ( Node(..)
    ) where


import Control.Monad


-- | Node Class.
-- The fundep indicates that the type of the input depends
-- on the type of the node. This is a massive win since then
-- the compiler knows to flag an error in nodes that have
-- ambiguous input types.

class (Functor m, Monad m) => Node i n (m :: * -> *) | n -> i
  where
      send :: Node i n m => n -> i -> m ()
      send output input = void $ fold output input
      fold :: Node i n m => n -> i -> m n
      fold node input = send node input >> return node
