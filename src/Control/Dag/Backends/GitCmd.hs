{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag.Backends.GitCmd where

import           Control.Applicative
import           Control.Monad.IO.Class
import           System.Exit
import           System.IO
import           System.Posix.Directory
import           System.Posix.Files
import           System.Process


data GitHeader = GitHeader
    { oPath_ :: FilePath
    , oSha1_ :: String
    } deriving (Eq, Ord, Read, Show)


data GitOutput o = GitOutput
    { oHeader_ :: GitHeader
    , oBody_   :: o
    } deriving (Show)


class (Functor m, MonadIO m) => HasGit m

instance HasGit IO


gitExists :: HasGit m => FilePath -> m Bool
gitExists = liftIO . fileExist


gitCommit :: HasGit m => FilePath -> String -> String -> m ()
gitCommit path msg body = liftIO $ do
    system' $ "mkdir -p `dirname \"" ++ path ++ "\"`"
    writeFile path body
    _ <- gitExec ["add", path]
    _ <- gitExec ["commit", "-m", msg]
    return ()


gitHeader :: HasGit m => FilePath -> m GitHeader
gitHeader path = GitHeader path <$> gitSha1 path


gitMessage :: HasGit m => FilePath -> m String
gitMessage path = let args = ["log", "-n 1", "--pretty=format:%B", path]
                  in liftIO $ gitExec args >>= hGetContents


gitReadOutput :: (HasGit m, Read a) => FilePath -> m (GitHeader, a)
gitReadOutput path = do
    contents <- liftIO $ readFile path
    header <- GitHeader path <$> gitSha1 path
    return $ (,) header $! read contents


gitSha1 :: (HasGit m) => FilePath -> m String
gitSha1 path = let args = ["log", "-n 1", "--pretty=format:%H", path]
               in liftIO $ gitExec args >>= hGetLine


gitExec :: [String] -> IO Handle
gitExec args = do
    let procArgs = (proc "git" args) { std_out = CreatePipe, std_err = Inherit }
    (_, Just h, _, p) <- createProcess procArgs
    rc <- waitForProcess p
    return $ checkRc ("git":args) h rc


system' :: String -> IO ()
system' cmd = checkRc cmd () <$> system cmd


checkRc :: Show a => a -> b -> ExitCode -> b
checkRc thing b rc = case rc of
    ExitFailure c -> error $ show thing ++ " `exited with` " ++ show c
    ExitSuccess -> b


withGit :: FilePath -> IO a -> IO a
withGit path effect = do
    wd <- getWorkingDirectory
    system' $ "mkdir -p " ++ path
    changeWorkingDirectory path
    effect <* changeWorkingDirectory wd
