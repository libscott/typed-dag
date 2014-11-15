{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}


module Control.Dag.Evented where


import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Control.Dag.Types.Node



class Emitter i r s (m :: * -> *) where
    addSubscriber :: Constructor (i -> r) (Subscribers s m) -> (r -> m ()) -> Subscribers s m -> Subscribers s m
    getSubscribers :: Constructor (i -> r) (Subscribers s m) -> Subscribers s m -> [r -> m ()]



-- The reason for a second monad here is that we build the dag in StateT
-- and run it in ReaderT

subscribe :: (Emitter i r s m8, Monad m, Node r n m8, MonadState (Subscribers s m8) m)
          => n -> Constructor (i -> r) (Subscribers s m8) -> m ()
subscribe node constructor = modify $ addSubscriber constructor (send node)


instance (Functor m, Emitter i r s m, MonadReader (Subscribers s m) m) =>
         Node i (Constructor (i -> r) (Subscribers s m)) m where
    send th@(Constructor constructor) input = do
        st <- ask
        let subscribers = getSubscribers th st
        -- forM_ subscribers $ \s -> s (constructor input)
        mapM_ ($ constructor input) subscribers


newtype Subscribers s (m :: * -> *) = Subscribers s
-- a wrapper for input to reference input type and state type
newtype Constructor e s = Constructor e




--
-- type Builder = StateT
--
--
-- type Evented = ReaderT
--
--
-- buildEvented :: Monad m => s -> Builder s m () -> m s
-- buildEvented = flip execStateT
--
--
-- runEvented :: Monad m => s -> Evented s m a -> m a
-- runEvented = flip runReaderT
--
--
