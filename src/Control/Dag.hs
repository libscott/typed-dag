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


import Data.Monoid



data PrinterNode g = PrinterNode g
instance (Functor m, Monad m, MonadIO m, Show i) => Node i (PrinterNode i) m where
    send _ = liftIO . print



input2 :: Many2 String ()
input2 = mempty



demo :: IO ()
demo = flip evalNodeState emptyDag $ do
    combiner <- addStateNode $ JunctionNode input2 $ PrinterNode input2
    send combiner $ OneOf2a "Hello"
    send combiner $ OneOf2b ()
