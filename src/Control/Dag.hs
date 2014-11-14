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



data PrinterNode g = PrinterNode g
instance (Functor m, Monad m, MonadIO m, Show i) => Node i (PrinterNode i) m where
    send _ = liftIO . print



type Inputs = Many2 String ()



demo :: IO ()
demo = flip evalNodeState emptyDag $ do
    combiner <- addStateNode $ junction2 $ PrinterNode (undefined::Inputs)
    send combiner $ OneOf2a "Hello"
    send combiner $ OneOf2b ()
