{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}


module Control.Dag.Types.Pull
    ( Output
    , Pull1 (..)
    , Pull2 (..)
    , Pull3 (..)
    ) where


class Monad m => Pull1 n o (m :: * -> *) | n -> o
  where
      pull1 :: n -> n
      pull1 = id
      execute1 :: n -> (o -> m ()) -> m ()


class Monad m => Pull2 n oa ob (m :: * -> *) | n -> oa, n -> ob
  where
    pull2 :: n -> ( Output oa m
                  , Output ob m
                  )
    pull2 node = ( Output (\f -> execute2 node f null')
                 , Output (\f -> execute2 node null' f)
                 )
    execute2 :: n -> (oa -> m ()) -> (ob -> m ()) -> m ()


class Monad m => Pull3 n oa ob oc (m :: * -> *) | n -> oa, n -> ob, n -> oc
  where
    pull3 :: n -> ( Output oa m
                  , Output ob m
                  , Output oc m
                  )
    pull3 node = ( Output (\f -> execute3 node f null' null')
                 , Output (\f -> execute3 node null' f null')
                 , Output (\f -> execute3 node null' null' f)
                 )
    execute3 :: n -> (oa -> m ()) -> (ob -> m ()) -> (oc -> m ()) -> m ()


-- | Output runs an upstream and proxies just one of it's outputs
data Output o (m :: * -> *) = Output ((o -> m ()) -> m ())


instance (Functor m, Monad m) => Pull1 (Output oa m) oa m where
    execute1 (Output f) = f


null' :: Monad m => a -> m ()
null' _ = return ()
