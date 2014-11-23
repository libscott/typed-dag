{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Backends.GitCmd where


import           System.IO
import           System.Posix.Directory
import           System.Process

import           Control.Dag.Prelude
import           Control.Dag.Types
import           Control.Dag.Utils


data GitException = FileNotVersioned deriving (Show, Eq, Enum)


gitCommit :: App m => String -> [FilePath] -> m ()
gitCommit msg paths = mapM_ (\path -> gitExec ["add", path]) paths
                   >> gitExec ["commit", "-m", msg]
                   >> return ()


gitMessage :: App m => String -> m String
gitMessage key = let args = ["log", "-n 1", "--pretty=format:%B", key]
                 in gitExec args >>= liftIO . hGetContents


gitInputHeader :: App m => FilePath -> m (Maybe InputHeader)
gitInputHeader path = fmap (InputHeader path) <$> gitCommitId path


gitCommitId :: App m => FilePath -> m (Maybe GitCommitId)
gitCommitId path = do
    let args = ["log", "-n 1", "--pretty=format:%H", path]
    gitExec args >>= liftIO . hMaybe (fmap GitCommitId . hGetLine)


gitRevList :: App m => String -> m [GitCommitId]
gitRevList expr = let args = ["rev-list", expr] in
    map GitCommitId . lines <$> (gitExec args >>= liftIO . hGetContents)


gitFilesAffected :: App m => GitCommitId -> m [FilePath]
gitFilesAffected (GitCommitId sha) = do
    let getFiles = gitExec ["log", "--name-status", "--oneline", sha]
    files <- lines <$> (getFiles >>= liftIO . hGetContents)
    dropLen <- length <$> view pathPrefix_
    return $ over each (drop dropLen) files


gitExec :: App m => [String] -> m Handle
gitExec args = do
    debug1 "GIT %v" (show args)
    liftIO $ do
        let procArgs = (proc "git" args) { std_out = CreatePipe, std_err = Inherit }
        (_, Just h, _, p) <- createProcess procArgs
        rc <- waitForProcess p
        return $ checkRc ("git":args) h rc


withGit :: (Applicative m, MonadIO m) => FilePath -> m a -> m a
withGit path effect = do
    wd <- liftIO $ getWorkingDirectory
                 <* changeWorkingDirectory path
                 -- check we're at GIT directory base dir
                 <* system' "[[ -d .git ]]"
    effect <* liftIO (changeWorkingDirectory wd)


hMaybe :: (Handle -> IO a) -> Handle -> IO (Maybe a)
hMaybe action h = do
    eof <- hIsEOF h
    if eof then return Nothing else Just <$> action h
