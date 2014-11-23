module Control.Dag.Prelude
    ( module Control.Applicative
    , module Control.Lens
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans
    , module Control.Monad.Trans.Control
    , module Control.Monad.Trans.Resource
    , module Data.ByteString
    , module Data.ByteString.Char8
    , module Data.Conduit
    , module Data.Conduit.List
    , module Data.Conduit.Binary
    , module Data.Either
    , module Data.List
    , module Data.Maybe
    , module System.Posix.Files
    , module Text.Printf
    ) where


-- | Export some useful stuff without clobbering the namespace
--   Foreign modules only


import           Control.Applicative
import           Control.Lens hiding (Context, index, parts, prism)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Conduit
import           Data.Conduit.List (consume, sourceList)
import           Data.Conduit.Binary (sourceFile, sinkFile)
import           Data.Either
import           Data.List (sort)
import           Data.Maybe (fromMaybe)
import           System.Posix.Files (fileExist)
import           Text.Printf
