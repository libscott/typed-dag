module Control.Dag.Utils where


import           Control.Monad.IO.Class
import           System.IO
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Printf



doLog :: MonadIO m => Priority -> String -> m ()
doLog priority' msg = liftIO $ logM "yea" priority' msg


info0 :: MonadIO m => String -> m ()
info0 = doLog INFO


info1 :: (MonadIO m, PrintfArg a) => String -> a -> m ()
info1 msg a0 = info0 $ printf msg a0


info2 :: (MonadIO m, PrintfArg a0, PrintfArg a1) => String -> a0 -> a1 -> m ()
info2 msg a0 a1 = info0 $ printf msg a0 a1


error0 :: MonadIO m => String -> m ()
error0 = doLog ERROR


setupLogging :: Priority -> IO ()
setupLogging level = do
    let f = simpleLogFormatter "$msg"
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
