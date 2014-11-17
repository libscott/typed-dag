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
    , WorkNode1 (..)
    ) where


data Node = N n f



data WorkNode1 a o m = WorkNode
    { getWork1 :: Pull1
    , getOutputs1 :: ()
    }


data WorkNode2 = WorkNode2



data Pull1 o m = Pull1
    { execute1 :: (n -> (o -> m ()) -> m ())
    }


data Pull2 oa ob m = Pull2
    { execute2 :: (n -> (oa -> m ()) -> (ob -> m ()) -> m ())
    }


data Pull3 oa ob oc m = Pull3
    { execute3 :: (n -> (oa -> m ()) -> (ob -> m ()) -> (oc -> m ()) -> m ())
    }


collect1 :: Pull1



pull2 :: Pull2 n oa ob m -> (Pull1 n oa m, Pull1 n ob m)
pull2 (Pull2 execute) = Pull1 $ \n
)

    (\f -> execute2 node f null')
             , Output (\f -> execute2 node null' f)
             )


pull3 node = ( Output (\f -> execute3 node f null' null')
             , Output (\f -> execute3 node null' f null')
             , Output (\f -> execute3 node null' null' f)
             )




null' :: Monad m => a -> m ()
null' _ = return ()
