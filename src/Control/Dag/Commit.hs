module Control.Dag.Commit where

import           Data.ByteString

import           Control.Dag.Backends.GitCmd
import           Control.Dag.Prelude
import           Control.Dag.Types
import           Control.Dag.Utils


-- | Should really be a pipe of pipes for maximum lazy
commitSink :: App m => FilePath -> InputVersions -> Sink ByteString m ()
commitSink path inputVers = lift (checkCreateDir path) >> f 1
  where
    message = show inputVers
    f i = do
        moutput <- await
        case moutput of
            Nothing -> return ()
            Just output -> do
                let partPath = printf "%v.%v" path i
                sourceList [output] $$ sinkFile partPath
                lift $ gitCommit partPath message
                f (i+1::Int)


checkCreateDir :: App m => FilePath -> m ()
checkCreateDir path =
    liftIO $ system' $ "mkdir -p `dirname \"" ++ path ++ "\"`"
