{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag
    ( demo
    ) where


import           Control.Monad.IO.Class

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





build :: IO (MySubscribers IO)
build = buildEvented s $ do
    subscribe PrinterNode $ construct s MyInput
    subscribe PrinterNode $ construct s MyInput
    subscribe PrinterNode $ construct s MyInput
  where
    s = Subscribers []



demo :: IO ()
demo = do
    subs <- build
    runEvented subs $ send (construct subs MyInput) "hello world"
