{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag
    ( Cont
    , Dag
    , demo
    ) where


import           Control.Monad
import           Control.Monad.State
import           Data.Dynamic

import Control.Dag.Node.ContNode
import Control.Dag.Node.StateNode
import Control.Dag.Types.Node


noDupes :: (Monad m, Eq a) => (a -> Cont a m ()) -> a -> Cont a m ()
noDupes f previous = do
    current <- await
    when (current /= previous) (f current)
    noDupes f current


fizzBuzz :: (Node n a m, TypeableNode n a m, MonadIO m) => StateNodeT m (StateNode n a (t m))
fizzBuzz = do
    printer <- addCont $ noDupes (liftIO . putStr) "_"
    let fizzybuzzy x s n = when (mod n x == 0) (send printer s)
    fizz <- addSimple $ fizzybuzzy 3 "Fizz"
    buzz <- addSimple $ fizzybuzzy 5 "Buzz"
    return undefined
    -- addSimple $ \n -> do
    --     send fizz n
    --     send buzz n
    --     send printer "\n"
  where
    forever_ :: Monad m => m a -> m ()
    forever_ = forever
    addCont :: MonadState DagState m => Cont a m () -> m (StateNode (Cont a m ()) a (t m))
    addCont = addNode . Ready

    addSimple f = addCont $ forever_ $ await >>= f


demo :: IO ()
demo = do
    (fb, st) <- runStateNodeT fizzBuzz emptyDag
    resultDag <- foldM (\a b -> execStateNodeT (send fb b) a) st [1..100]
    print resultDag
