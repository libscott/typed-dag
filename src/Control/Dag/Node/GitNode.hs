{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Node.GitNode where


import Control.Monad
import Control.Monad.State.Strict
import Data.Monoid
import Data.Maybe

import Control.Dag.Types.Node



data GitNode i s n = GitNode n


instance (Monoid s, NamedNode i n m, Node i n m) => Node i (GitNode n) m where
    send (GitNode node) input = do




data GitOutputNode = GitOutputNode




data GitNode i n = GitNode i n deriving Show
