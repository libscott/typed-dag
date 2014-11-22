module Control.Dag.Utils where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           System.Exit
import           System.IO
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Process
import           Text.Printf

import           Control.Dag.Types


-------------------
-- Data
-------------------


-- | Clean and concatenate file paths
fixPaths :: [FilePath] -> FilePath
fixPaths [] = []
fixPaths (x : xs) = stripr x ++ concatMap clean xs
    where
      clean = ('/':) . stripr . stripl
      stripr = reverse . stripl . reverse
      stripl = dropWhile (=='/')


-----------------
-- | Logging
-----------------


doLog :: App m => Priority -> String -> m ()
doLog priority' msg = do
    name <- view loggerName_
    liftIO $ logM name priority' msg


info0 :: App m => String -> m ()
info0 = doLog INFO


info1 :: (App m, PrintfArg a) => String -> a -> m ()
info1 msg a0 = info0 $ printf msg a0


info2 :: (App m, PrintfArg a0, PrintfArg a1) => String -> a0 -> a1 -> m ()
info2 msg a0 a1 = info0 $ printf msg a0 a1


error0 :: App m => String -> m ()
error0 = doLog ERROR


setupLogging :: Priority -> IO ()
setupLogging level = do
    let f = simpleLogFormatter "$prio $time $loggername $msg"
    handler <- streamHandler stderr level
    let h = setFormatter handler f
    updateGlobalLogger rootLoggerName $
        addHandler h .
        removeHandler .
        System.Log.Logger.setLevel level


infoLogging :: IO ()
infoLogging = setupLogging INFO


debugLogging :: IO ()
debugLogging = setupLogging DEBUG


---------------------
-- | Processes
---------------------

system' :: String -> IO ()
system' cmd = checkRc cmd () <$> system cmd


checkRc :: Show a => a -> b -> ExitCode -> b
checkRc thing b rc = case rc of
    ExitFailure c -> error $ show thing ++ " `exited with` " ++ show c
    ExitSuccess -> b


--------------------
-- | Crypto
--------------------


-- sha1file :: MonadIO m => FilePath -> m Sha1
-- sha1file = hashFile


---------------------
-- | Conduits
---------------------


clMap :: MonadIO m => (a -> b) -> Conduit a m b
clMap = CL.map
