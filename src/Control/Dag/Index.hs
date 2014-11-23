module Control.Dag.Index
    ( getPathSubscribers
    , makePathSubscribers
    ) where


import           Data.List (nub, sort)
import qualified Data.Map.Strict as Map

import           Control.Dag.Types




makePathSubscribers :: Subscriber m -> PathSubscribers m
makePathSubscribers = Map.map (sort . nub) . Map.fromListWith (++) . f
  where
    f n@(GitNode _ inputs _) = let tail' = concatMap f inputs
                               in [(gPath_ i, [n]) | i <- inputs] ++ tail'


getPathSubscribers :: FilePath -> PathSubscribers m -> [Subscriber m]
getPathSubscribers path = maybe [] id . Map.lookup path
