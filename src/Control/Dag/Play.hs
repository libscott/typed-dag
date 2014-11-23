module Control.Dag.Play where


import           Control.Applicative
import           Control.Lens ((^.))
import           Control.Monad.State.Strict
import           Data.List (isSuffixOf)
import           System.IO

import           Control.Dag.Types
import           Control.Dag.Backends.GitCmd
import           Control.Dag.Index
import           Control.Dag.Utils
import           Control.Dag.Prelude



type Player = IO


--
-- Two modes of operation:
--
-- Recursive mode is when you start with the output you
-- want and it recurses upstream to generate it for you.
--
-- Evented mode is when a commit's outputs are examined and
-- any subscribers to that output are in turn called.
--
-- In order to do evented mode we will need to query the db to see
-- what outputs were affected by the commit, so we should look for
-- .output files.
--

-- replay strategy:
-- iterate commits. Check files of each commit. If any file ends with .output,
-- check the subscriber map for nodes that might be subscribing and run them.
-- thats it.


play :: App m => PathSubscribers m -> m ()
play index = do
    commitId <- GitCommitId <$> liftIO (withFile "OFFSET" ReadMode hGetLine)
    mnext <- gitGetNextCommit $! commitId
    info2 "Playing at %s, next is %s" (show commitId) (show mnext)
    case mnext of
        Nothing  -> return ()
        Just cid@(GitCommitId sha) -> do
            checkReplayCommit index cid
            liftIO $ writeFile "OFFSET" sha
            play index


checkReplayCommit :: App m => PathSubscribers m -> GitCommitId -> m ()
checkReplayCommit index cid = do
    affected <- gitFilesAffected cid
    let present = [drop 1 l | l <- affected, head l /= 'D']
        stripped = map (dropWhile (==' ')) present
        filtered = filter (isSuffixOf ".output") stripped
    -- for each output of this job, find what depends on that output and
    -- trigger it.
    let subscribers = concatMap (`getPathSubscribers` index) filtered
    liftIO $ print $ length subscribers
    mapM_ (^.runner_) subscribers


-- given the last commit we processed, get the next commit to process based
-- on the head of the repository. Stupidly easy.
gitGetNextCommit :: App m => GitCommitId -> m (Maybe GitCommitId)
gitGetNextCommit (GitCommitId offset) = do
    list <- gitRevList $ offset ++ "..HEAD"
    return $ if null list then Nothing
                          else Just $ last list


data CompareInputsResult = Changed | Same | DoesNotExist


compareInputs :: App m => FilePath -> InputVersions -> m CompareInputsResult
compareInputs path inputVers = do
    exist <- liftIO $ fileExist path
    if exist then do changed <- compareVers inputVers
                     return $ if changed then Changed else Same
             else return DoesNotExist

  where
    compareVers new = do
        old <- read <$> gitMessage path
        return $ old /= new
