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
    , SubscriberEdge
    , SubscriberIndex
    , pathPrefix_
    , loggerName_
    , path_
    , inputs_
    , runner_
    , sha1_
    ) where


import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.List (sort)



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
      ) => App m


instance ( Applicative m, MonadIO m, Functor m
         , MonadThrow m, MonadResource m
         ) => App (ReaderT Context m)


---------------------
-- | Indexer
---------------------


-- | node -> [list of nodes that depend on it's outputs]
-- I guess this might not technically be an "edge", it's the set of edges
-- pointing to the producing node.
type SubscriberEdge m = (GitNode m (), [GitNode m ()])


type SubscriberIndex m = [SubscriberEdge m]
