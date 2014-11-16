{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Node.GitNode where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           System.Exit
import           System.IO
import           System.Posix.Directory
import           System.Posix.Files
import           System.Process


import           Control.Dag.Node.JobNode
import           Control.Dag.Types.Node


-- TODO: Please for the love of god use libgit2. MonadGitBackend etc.


data Ping = Ping deriving Show


data GitNode i n = GitNode
    { gPath_       :: FilePath
    , gInputPaths_ :: [FilePath]
    , gDownstream_ :: n
    } deriving (Show)


data GitCommit = GitCommit
    { cSha1_    :: String
    , cMessage_ :: String
    } deriving (Show)


instance (MonadIO m, Node (JobArgs i) n m) => Node Ping (GitNode i n) m where
    send = ping


-- | Notify a Git node that something may have changed
ping :: (MonadIO m, Node (JobArgs i) n m) => GitNode i n -> Ping -> m ()
ping (GitNode path inputPaths downstream) Ping = do
    mMyCommit <- runMaybeT $ getInputCommit path
    mJobArgs <- runMaybeT $ do
        inputCommits <- mapM getInputCommit inputPaths
        let newMsg = show (map cSha1_ inputCommits) ++ "\n"
            onOutput = liftIO . commitOutput newMsg
            jobArgs = JobArgs path inputPaths onOutput
            mChanged = fmap ((/=newMsg) . cMessage_) mMyCommit
        if mChanged /= Just False then return jobArgs else mzero
    maybe (return ()) (send downstream) mJobArgs


commitOutput :: String -> FilePath -> IO ()
commitOutput msg path = do
    _ <- execGit ["add", path]
    _ <- execGit ["commit", "-m", msg]
    return ()


getInputCommit :: MonadIO m => FilePath -> MaybeT m GitCommit
getInputCommit path = do
    exists <- liftIO $ fileExist path
    if exists then getCommit path else mzero


getCommit :: MonadIO m => FilePath -> MaybeT m GitCommit
getCommit path = do
    h <- liftIO gitLog
    eof <- liftIO $ hIsEOF h
    if eof then mzero
           else liftIO $ GitCommit <$> hGetLine h <*> hGetContents h
  where
    gitLog = execGit ["log", "-n 1", "--pretty=format:%H%n%B", path]


execGit :: [String] -> IO Handle
execGit args = do
    let procArgs = (proc "git" args) { std_out = CreatePipe }
    (_, Just h, _, p) <- createProcess procArgs
    rc <- waitForProcess p
    case rc of
        ExitFailure c -> error $ "Git exited with " ++ show c
        ExitSuccess -> return h


withGit :: FilePath -> IO a -> IO a
withGit path effect = do
    wd <- getWorkingDirectory
    changeWorkingDirectory path
    effect <* changeWorkingDirectory wd
