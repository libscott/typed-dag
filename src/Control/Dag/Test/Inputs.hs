{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE KindSignatures             #-}



module Control.Dag.Test.Inputs where

import Control.Dag.Evented




data MyInput = MyInput String deriving (Show)

type MySubscribers (m :: * -> *) = Subscribers [MyInput -> m ()] m



instance Emitter (Constructor (String -> MyInput) String MyInput [MyInput -> m ()])
                 String MyInput ([MyInput -> m ()]) m where
    addSubscriber _ send' (Subscribers s) = Subscribers $ s ++ [send']
    -- getSubscribers :: Constructor (i -> r) (Subscribers s m) -> Subscribers s m -> [r -> m ()]
    getSubscribers _ (Subscribers s) = s
    wrap (Constructor f) = f
