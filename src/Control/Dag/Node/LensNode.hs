{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}



module Control.Dag.Node.LensNode where


import           Control.Lens
import           Control.Dag.Types.Node


data LensNode l r i n = LensNode (Lens r r i i) n


instance Node i n m => Node r (LensNode l r i n) m where
    fold (LensNode lenz node) = undefined -- lenz #%%~ send node
