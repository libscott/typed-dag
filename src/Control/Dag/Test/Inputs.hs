{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}


module Control.Dag.Test.Inputs where


import Control.Lens
import Control.Monad.State.Strict


data MySubscribers m = MySubscribers
    { _fStrings :: [String -> StateT (MySubscribers m) m ()]
    }

makeFields ''MySubscribers
