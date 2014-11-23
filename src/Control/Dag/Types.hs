{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}

module Control.Dag.Types
    ( App
    , Context (..)
    , Algorithm (..)
    , AlgoVersion
    , InputHeader (..)
    , InputVersions (..)
    , GitNode (..)
    , GitCommitId (..)
    , Subscriber
    , PathSubscribers
    , pathPrefix_
    , loggerName_
    , path_
    , inputs_
    , runner_
    , sha1_
    ) where


import           Control.Monad.Reader
import qualified Data.Map as Map

import           Control.Dag.Prelude



type AlgoVersion = String


data Algorithm f m = Algorithm f (m AlgoVersion)


newtype GitCommitId = GitCommitId String deriving (Eq, Ord, Read, Show)


data InputVersions = InputVersions AlgoVersion [InputHeader]
    deriving (Read, Show)


instance Eq InputVersions where
    (==) (InputVersions algo heads) (InputVersions algo' heads') =
            algo == algo' && sort heads == sort heads'


data InputHeader = InputHeader
    { hPath_ :: FilePath
    , hSha1_ :: GitCommitId
    } deriving (Eq, Ord, Read, Show)


makeFields ''InputHeader


data GitNode (m :: * -> *) o = GitNode
    { gPath_   :: FilePath
    , gInputs_ :: [GitNode m ()]
    , gRunner_ :: m (InputHeader, Source m o)
    }


instance Eq (GitNode m o) where
    (GitNode path1 _ _) == (GitNode path2 _ _) = path1 == path2


instance Ord (GitNode m o) where
    (GitNode path1 _ _) <= (GitNode path2 _ _) = path1 <= path2


instance Show (GitNode m o) where
    show (GitNode path inputs _) = "GitNode " ++ show path ++ " " ++ show inputs


makeFields ''GitNode


---------------------
-- | Runtime
---------------------


data Context = Context
    { sPathPrefix_ :: FilePath -- prefix a string to all FilePaths
    , sLoggerName_ :: String
    } deriving (Show)


makeFields ''Context


class ( Applicative m, MonadReader Context m
      , MonadIO m, Functor m, MonadResource m
      , MonadBaseControl IO m
      ) => App m


instance ( Applicative m, MonadIO m, Functor m
         , MonadThrow m, MonadResource m
         , MonadBaseControl IO m
         ) => App (ReaderT Context m)


---------------------
-- | Indexer
---------------------

type Subscriber m = GitNode m ()


type PathSubscribers m = Map.Map FilePath [Subscriber m]
