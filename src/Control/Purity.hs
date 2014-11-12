{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


-- | Purity: a type graph and executor for complex application flow
-- builds data structure made of Nodes.


module Control.Purity where


import           Control.Monad.State

import           Control.Purity.Internal.Cont
import           Control.Purity.Internal.Dag





main :: IO ()
main = runDag fizzBuzz >>= print


fizzBuzz :: Dag IO ()
fizzBuzz = do
    printer <- addSimple $ liftIO . putStr
    let send_ nid = lift . send nid
    let fizzybuzzy x s n = when (mod n x == 0) (send_ printer s)
    fizz <- addSimple $ fizzybuzzy 3 "Fizz"
    buzz <- addSimple $ fizzybuzzy 5 "Buzz"
    fizzbuzz <- addSimple (\n -> send_ fizz n >> send_ buzz n >> send_ printer "\n")
    mapM_ (send fizzbuzz) nums
  where
    forever_ :: Monad m => m a -> m ()
    forever_ = forever
    addSimple f = addCont $ forever_ $ await >>= f
    nums :: [Int]
    nums = [1..100]
