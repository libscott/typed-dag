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



type Printer = NodeId (Cont String (Dag IO) ()) String


main :: IO ()
main = runDag fizzBuzz >>= print


fizzBuzz :: Dag IO ()
fizzBuzz = do
    printer <- addSimple $ liftIO . putStr :: Dag IO (Printer (IO ()))
    let send_ nid = lift . send nid
    let fizzybuzzy x s n = when (mod n x == 0) (send_ printer s)
    fizz <- addSimple $ fizzybuzzy 3 "Fizz"           :: Dag IO (NodeId (Cont Int (Dag IO) ()) Int (IO ()))
    buzz <- addSimple $ fizzybuzzy 5 "Buzz"           :: Dag IO (NodeId (Cont Int (Dag IO) ()) Int (IO ()))
    fizzbuzz <- addSimple (\n -> send_ fizz n >> send_ buzz n >> send_ printer "\n") :: Dag IO (NodeId (Cont Int (Dag IO) ()) Int (IO ()))
    mapM_ (send fizzbuzz) [1..100]
  where
    addSimple f = addCont $ forever $ await >>= f
