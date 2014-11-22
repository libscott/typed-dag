module Control.Dag.Prelude
    ( module Control.Applicative
    , module Control.Lens
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans
    , module Control.Monad.Trans.Resource
    , module Data.ByteString
    , module Data.ByteString.Char8
    , module Data.Conduit
    , module Data.Conduit.List
    , module Data.Conduit.Binary
    , module Text.Printf
    ) where


-- | Export some useful stuff without clobbering the namespace


import           Control.Applicative
import           Control.Lens hiding (Context, parts)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Conduit
import           Data.Conduit.List (consume, sourceList)
import           Data.Conduit.Binary (sourceFile, sinkFile)
import           Text.Printf
