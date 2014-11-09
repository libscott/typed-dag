{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


-- | Purity: a type graph and executor for complex application flow
-- builds data structure made of Nodes.


module Control.Purity
    ( test
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Dynamic
import qualified Data.IntMap as IntMap
import           Data.Tuple


type DagT = StateT (IntMap.IntMap Dynamic)

newtype STID = STID Int deriving (Show)

data Node a b s m r = Node (a -> s -> m (b, s)) r STID


instance (Show s, Show r) => Show (Node a b s m r) where
    show (Node _ s child) = "Node " ++ show s ++ " " ++ show child


class Runnable r a m where -- ActiveNode
    run :: r -> a -> DagT m ()


getState :: (Functor m, Monad m, Typeable a) => STID -> DagT m a
getState (STID stid) = flip fromDyn undefined . (IntMap.! stid) <$> get


putState :: (Monad m, Typeable a) => STID -> a -> DagT m ()
putState (STID stid) = modify . IntMap.insert stid . toDyn


addState :: (Functor m, Monad m, Typeable a) => a -> DagT m STID
addState x = do
    map' <- get
    let i = IntMap.size map'
    put $ IntMap.insert i (toDyn x) map'
    return $ STID i


addNode :: (Functor m, Monad m, Typeable s, Runnable r a m) => (a -> s -> m (b, s)) -> r -> s
       -> DagT m (Node a b s m r)
addNode f r s = Node f r <$> addState s


instance (Functor m, Monad m, Runnable r b m, Typeable s) =>
         Runnable (Node a b s m r) a m
  where
    run (Node f child stid) input = do
        st <- getState stid
        (out, st') <- lift $ f input st
        putState stid st'
        run child out


addBatchingNode :: (Functor m, Monad m, Typeable a, Runnable r a m)
                => Int -> r -> DagT m (Node a [a] [a] m r)
addBatchingNode n output = addNode batch output []
  where
    batch item buf = let res = (item:buf, []) in
                     return $ if length buf == n - 1 then res else swap res


data ListPrinter = ListPrinter

instance Show a => Runnable ListPrinter a IO
  where
    -- run _ [] = return ()
    run _ a = lift $ print a


test :: DagT IO ()
test = do
    batchingNode <- addBatchingNode 5 ListPrinter :: DagT IO (Node Int [Int] [Int] IO ListPrinter)
    producer1 <- addNode (\a s -> return (s, s + a)) batchingNode 0 :: DagT IO (Node Int Int Int IO (Node Int [Int] [Int] IO ListPrinter))
    producer2 <- addNode (\a s -> return (s, s - a)) batchingNode 0 :: DagT IO (Node Int Int Int IO (Node Int [Int] [Int] IO ListPrinter))
    mapM_ (\i -> run producer1 i >> run producer2 i) ([1..100] :: [Int])


main :: IO ()
main = runStateT test IntMap.empty >>= print
