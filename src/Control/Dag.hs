{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE KindSignatures             #-}


module Control.Dag
    ( demo
    ) where


import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Control.Dag.Node.JunctionNode
import           Control.Dag.Node.StateNode
import           Control.Dag.Types.Node

import           Control.Dag.Prelude
import           Control.Dag.Evented
import           Control.Dag.Test.Inputs


import           Data.Monoid



data PrinterNode s = PrinterNode


instance (Functor m, MonadIO m, Show s) => Node s (PrinterNode s) m where
    send _ = liftIO . print


type MySubscribers (m :: * -> *) = [String -> m ()]


dag :: (Functor m, MonadState (Subscribers (MySubscribers m) m) m, MonadIO m) => m ()
dag = do
    s <- get
    let emitMyInput = makeEmitter s id
    subscribe PrinterNode emitMyInput
    subscribe PrinterNode emitMyInput
    subscribe PrinterNode emitMyInput
    send emitMyInput "hello world"


-- instance (Functor m, Monad m, MonadState (Subscribers s m) m) => Node i (Emitter i s m) m



demo :: IO ()
demo = do
    runEvented dag mempty
    return ()
