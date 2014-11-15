{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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





data PrinterNode s = PrinterNode


instance (Functor m, MonadIO m, Show s) => Node s (PrinterNode s) m where
    send _ = liftIO . print



dag :: StateT (MySubscribers IO) IO ()
dag = do
    emitMyInput <- makeEmitter strings
    subscribe PrinterNode emitMyInput
    send emitMyInput "hello world"


demo :: IO ()
demo = do
    _ <- runEvented dag $ MySubscribers []
    return ()
