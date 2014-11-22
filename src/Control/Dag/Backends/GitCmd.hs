{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Backends.GitCmd where


import qualified Data.Conduit.List as CL
import           System.IO
import           System.Posix.Directory
import           System.Posix.Files
import           System.Process

import           Control.Dag.Prelude
import           Control.Dag.Types
import           Control.Dag.Utils


gitExists :: App m => FilePath -> m Bool
gitExists = liftIO . fileExist


gitCommit :: App m => FilePath -> String -> m ()
gitCommit path msg = gitExec ["add", path]
                  >> gitExec ["commit", "-m", msg]
                  >> return ()


gitHeader :: App m => FilePath -> m InputHeader
gitHeader path = InputHeader path <$> gitSha1 path


gitMessage :: App m => String -> m String
gitMessage key = let args = ["log", "-n 1", "--pretty=format:%B", key]
                 in gitExec args >>= liftIO . hGetContents


gitReadOutput :: (App m, Read a) => FilePath -> m (InputHeader, Source m a)
gitReadOutput path = do
    header <- InputHeader path <$> gitSha1 path
    return (header, sourceFile path $= CL.map (read . unpack))


gitSha1 :: App m => FilePath -> m GitCommitId
gitSha1 path = let args = ["log", "-n 1", "--pretty=format:%H", path] in
    GitCommitId <$> (gitExec args >>= liftIO . hGetLine)


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
gitExec args = liftIO $ do
    liftIO $ print args
    let procArgs = (proc "git" args) { std_out = CreatePipe, std_err = Inherit }
    (_, Just h, _, p) <- createProcess procArgs
    rc <- waitForProcess p
    return $ checkRc ("git":args) h rc


withGit :: (Applicative m, MonadIO m) => FilePath -> m a -> m a
withGit path effect = do
    wd <- liftIO $ getWorkingDirectory
                 <* changeWorkingDirectory path
                 <* system' "[[ -d .git ]]" -- check we're at GIT directory base dir
    effect <* liftIO (changeWorkingDirectory wd)
