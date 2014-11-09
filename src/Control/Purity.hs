{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Control.Purity
    (
    ) where

import Control.Monad

-- | Purity: a type graph and executor for complex application flow
-- builds data structure made of Nodes.
-- At the top you have the producers which wait for events



data Node a b s r = Node (s -> a -> IO (s, b)) s r

data Leaf a s = Leaf (s -> a -> IO s) s


instance (Show s, Show r) => Show (Node a b s r) where
    show (Node _ s child) = "Node " ++ show s ++ " " ++ show child


instance Show s => Show (Leaf a s) where
    show (Leaf _ s) = "Leaf " ++ show s


class Runnable r a where
    run :: r -> a -> IO r


instance Runnable r b => Runnable (Node a b s r) a where
    run (Node f state child) input = do
        (state', out) <- f state input
        child' <- run child out
        return $ Node f state' child'


instance Runnable (Leaf a s) a where
    run (Leaf f state) input = do
        state' <- f state input
        return $ Leaf f state'


type AccumNode a = Leaf a [a]
type MaxStringLengthNode n = Node String Int Int n


main :: IO ()
main = do
    let max' a b = let r = max a (length b) in return (r, r)
    let accumNode = Leaf (\s a -> return (a:s)) []
    let maxNode = Node max' 0 accumNode :: MaxStringLengthNode (AccumNode Int)


    foldM run maxNode ["dddd", "a", "bb", "ccc"] >>= print
