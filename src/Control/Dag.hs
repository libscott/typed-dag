{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag
    ( demo
    ) where


import           Control.Dag.Types.Pull
import           Control.Monad.State.Strict


data A = A

data B a b = B a b


instance (Monad m) => Pull2 A Int String m where
    execute2 _ fa fb = fa 1 >> fb "String"


class (Pull1 n o m) => Collect1 n o m where
    collect1 :: n -> m o


instance (Pull1 n o m) => Collect1 n (Maybe o) m where
    collect1 node = execStateT (execute1 node put) (error "No output")





instance (Collect1 a Int m, Collect1 b String m, Functor m, Monad m) => Pull1 (B a b) Bool m
  where
    execute1 (B up1 up2) f = do
        int <- collect1 up1
        str <- collect1 up2
        f $ show int == str



dag = let (up1, up2) = pull2 A
       in B up1 up2



demo :: IO ()
demo = undefined
