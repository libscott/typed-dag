module Control.Dag.Commit where

import qualified Data.Conduit.List as CL

import           Control.Dag.Backends.GitCmd
import           Control.Dag.Prelude
import           Control.Dag.Types
import           Control.Dag.Utils


-- | Should really be a pipe of pipes for maximum lazy
--   Until theres a good reason to stream output paths, just return them.
--   Commit all output parts into the same commit.
commitSink :: App m => FilePath -> InputVersions -> Sink ByteString m ()
commitSink path inputVers = do
    lift (checkCreateDir path)
    (writeParts path >> writeOutput) =$ commitParts
  where
    writeOutput = do
        let p = path ++ ".output"
        sourceList [pack $ show msg] $$ sinkFile p
        yield p
    msg = show inputVers
    commitParts = consume >>= lift . gitCommit msg


writeParts :: App m => FilePath -> Conduit ByteString m FilePath
writeParts path = f $ partPaths path 1
  where
    -- dont like this.
    f (partPath:xs) = do
        moutput <- await
        case moutput of
            Nothing -> return ()
            Just output -> do
                sourceList [output] $$ sinkFile partPath
                yield partPath
                f xs


partPaths :: FilePath -> Int -> [FilePath]
partPaths path i = printf "%v.%v" path i : partPaths path (i+1)


sourceOutput :: (App m, Read a) => FilePath -> m (InputHeader, Source m a)
sourceOutput path = do
    header <- fromMaybe (e1 path) <$> gitInputHeader path
    return (header, parts)
  where
    parts = mapM_ sourceFile (partPaths path 1) =$ CL.map (read . unpack)
    e1 p = error $ "output somehow not versioned " ++ p


checkCreateDir :: App m => FilePath -> m ()
checkCreateDir path =
    liftIO $ system' $ "mkdir -p `dirname \"" ++ path ++ "\"`"
