module Control.Dag.Index
    ( SubscriberIndex
    , makeSubscriberIndex
    , getSubscribersByPath
    ) where


import           Control.Lens
import           Data.List -- Lets have some fun eh?

import Control.Dag.Types


getSubscribersByPath :: FilePath -> SubscriberIndex m -> [GitNode m ()]
getSubscribersByPath _    []                = []
getSubscribersByPath path ((node, subs):xs) =
    if node^.path_ == path then subs else getSubscribersByPath path xs


-- Turns a node -> parent tree into an "index" of parent -> node
makeSubscriberIndex :: GitNode m () -> [SubscriberEdge m]
makeSubscriberIndex node = foldl combineEdges [] sorted
  where
    sorted = sortBy (\(n,_) (n',_) -> compare n n') $ (each._2 %~ sort) edges
    edges = getEdges node
    getEdges n@(GitNode _ inputs _) =
        [(i, [n]) | i <- inputs] ++ concatMap getEdges inputs


combineEdges :: [SubscriberEdge m] -> SubscriberEdge m -> [SubscriberEdge m]
combineEdges []                    edge         = [edge]
combineEdges a@((node', deps'):xs) (node, deps) =
    if node == node' then (node, smerge deps deps'):xs
                     else (node, deps):a


-- merge 2 sorted lists, deduplicating
smerge :: (Ord a) => [a] -> [a] -> [a]
smerge xs [] = xs
smerge [] ys = ys
smerge (x : xs) (y : ys) = case compare x y of
    EQ -> x : smerge xs ys
    LT -> x : smerge xs (y : ys)
    GT -> y : smerge (x : xs) ys
