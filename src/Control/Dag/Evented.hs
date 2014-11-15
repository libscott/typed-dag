{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}


module Control.Dag.Evented where


import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Monoid

import           Control.Dag.Types.Node



-- | The reason for a second monad here is that we build the dag in StateT
-- and run it in StateT
subscribe :: (Monad m, Node i n m, MonadState (Subscribers s m) m)
          => n -> Emitter i s m -> m ()
subscribe node em = modify $ addSubscriber em $ send node




newtype Subscribers s (m :: * -> *) = Subscribers s deriving (Monoid)


data Emitter i s m = Emitter { addSubscriber :: (i -> m ()) -> Subscribers s m -> Subscribers s m
                             , getSubscribers :: Subscribers s m -> [i -> m ()]
                             }


makeEmitter :: Subscribers s m -> ALens s s [i -> m ()] [i -> m ()] -> Emitter i s m
makeEmitter _ lenz = Emitter
    (\send' (Subscribers s) -> Subscribers (s & lenz #%~ (send':)))
    (\(Subscribers s) -> s^# lenz)


instance (Functor m, Monad m, MonadState (Subscribers s m) m) => Node i (Emitter i s m) m where
    send (Emitter _ getSubs) input = get >>= mapM_ ($input) . getSubs


type Evented s m = StateT (Subscribers s m) m


runEvented :: Monad m => Evented s m a -> Subscribers s m -> m (a, Subscribers s m)
runEvented = runStateT
