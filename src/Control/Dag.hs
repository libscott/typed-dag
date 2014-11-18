{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE KindSignatures             #-}


module Control.Dag
    ( module Control.Dag.Utils
    , Algo (..)
    , AlgoVersion (..)
    , GitNode (..)
    , HasGit
    , codeHash
    , execute
    , gitNode0
    , gitNode1
    , gitNode2
    , withGit
    ) where


import           Control.Monad
import           Data.List (sort)

import           Control.Dag.Backends.GitCmd

import           Control.Dag.Algorithm
import           Control.Dag.Utils


execute :: GitNode m o -> m (GitHeader, o)
execute = gRunner_


data GitNode (m :: * -> *) o = GitNode
    { gPath_      :: FilePath
    , gInputs_    :: [FilePath]
    , gRunner_    :: m (GitHeader, o)
    }


gitNode0 :: (HasGit m, Read o, Show o) => FilePath -> Algo (m o) -> GitNode m o
gitNode0 path (Algo f v) = GitNode path [] $ do
    let newMsg = gitCommitMessage path v []
    gitCheckRunEffect path newMsg f
    gitReadOutput path


gitNode1 :: (HasGit m, Read o, Show o) => FilePath
                                       -> GitNode m a
                                       -> Algo (a -> m o)
                                       -> GitNode m o
gitNode1 path input (Algo f v) = GitNode path inputs $ do
    (depHead, body) <- gRunner_ input
    let newMsg = gitCommitMessage path v [depHead]
    gitCheckRunEffect path newMsg (f body)
    gitReadOutput path
  where
    inputs = [gPath_ input]


gitNode2 :: (HasGit m, Read o, Show o) => FilePath
                                       -> GitNode m a
                                       -> GitNode m b
                                       -> Algo ((a, b) -> m o)
                                       -> GitNode m o
gitNode2 path input1 input2 (Algo f v) = GitNode path inputs $ do
    (depHead1, body1) <- gRunner_ input1
    (depHead2, body2) <- gRunner_ input2
    let newMsg = gitCommitMessage path v [depHead1, depHead2]
    gitCheckRunEffect path newMsg (f (body1, body2))
    gitReadOutput path
  where
    inputs = [gPath_ input1, gPath_ input2]


gitCheckRunEffect :: (HasGit m, Show o) => FilePath -> String -> m o -> m ()
gitCheckRunEffect path newMsg effect = do
    info1 "%s: Called" path
    exists <- gitExists path
    go <- if exists
        then do changed <- compareMessage newMsg
                if changed
                    then do info "Inputs changed"
                            return True
                    else do info "No change in inputs"
                            return False
        else do info "Creating"
                return True
    when go $ do
        info "Running job"
        effect >>= commit . show
  where
    info s = info0 $ path ++ ": " ++ s
    commit = gitCommit path newMsg
    compareMessage new = do
        old <- gitMessage path
        return $ old /= (new ++ "\n")


gitCommitMessage :: FilePath -> AlgoVersion -> [GitHeader] -> String
gitCommitMessage path ver heads = show (path, ver, sort heads)
