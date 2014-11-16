{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Control.Dag
    ( demo
    ) where


import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Dag.Types.Node


-- import           Control.Dag.Node.EmitterNode
import           Control.Dag.Node.GitNode
import           Control.Dag.Node.JobNode
import           Control.Dag.Test.Inputs
import           System.IO


type Dag s m = StateT (s m) m


-- data PrinterNode i = PrinterNode
-- instance (Functor m, MonadIO m, Show i) => Node i (PrinterNode i) m where
--     send _ = liftIO . print


data MyJobNode = MyJobNode
instance (Functor m, MonadIO m) => Node (JobArgs String) MyJobNode m where
    send _ (JobArgs path myInputs complete) = liftIO $ do
        print (path, myInputs)
        withFile path AppendMode $ flip hPutStrLn "OK!"
        complete path
        return ()






dag :: Dag MySubscribers IO ()
dag = do
    -- emitMyInput <- makeEmitter strings
    let gitNode = GitNode "README" ["hello", "world"] MyJobNode
    send gitNode Ping



demo :: IO ()
demo = withGit "repo" $ evalStateT dag $ MySubscribers []
