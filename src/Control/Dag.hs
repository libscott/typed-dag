{-# LANGUAGE FlexibleContexts           #-}

module Control.Dag
    ( module Control.Dag.Utils
    , Algorithm (..)
    , AlgoVersion
    , GitNode (..)
    , App
    , codeHash
    , execute
    , fileInput
    , in0out1
    , in0out2
    , in1out1
    , in1out2
    , in2out1
    , in2out2
    , in3out1
    , in3out2
    , in3out3
    , playNode
    , runApp
    , script
    , terminate
    , watchedFile
    , withGit
    ) where


import           Control.Applicative
import           Control.Monad.Reader

import           Control.Dag.Algorithm
import           Control.Dag.Backends.GitCmd
import           Control.Dag.Build
import           Control.Dag.Index
import           Control.Dag.Play
import           Control.Dag.Prelude
import           Control.Dag.Runners
import           Control.Dag.Types
import           Control.Dag.Utils


playNode :: App m => GitNode m () -> m ()
playNode node = do
    let idx = makePathSubscribers node
    play idx


runApp :: (Applicative m, MonadIO m, MonadBaseControl IO m)
       => FilePath -> FilePath -> ReaderT Context (ResourceT m) a -> m a
runApp repoPath prefix effect =
    withGit repoPath $ runResourceT $ do
        let context = Context prefix "root"
        runReaderT effect context
