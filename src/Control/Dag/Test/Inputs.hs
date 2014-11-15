{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ExistentialQuantification  #-}



module Control.Dag.Test.Inputs where

import Control.Dag.Types.Node
import Control.Dag.Evented
import Data.Monoid




data MyInput i = MyInput String i


type MySubscribers m = Subscribers [MyInput -> m ()]


instance Emitter (i -> MyInput i) (MySubscribers m) m where
    addSubscriber :: Constructor (i -> r) (Subscribers s m) -> (r -> m ()) -> Subscribers s m -> Subscribers s m
    -- getSubscribers :: Constructor (i -> r) (Subscribers s m) -> Subscribers s m -> [r -> m ()]



m f a = a { f = f a }
