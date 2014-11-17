{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE KindSignatures             #-}


module Control.Dag
    ( demo
    ) where


import           Control.Applicative
import           Control.Dag.Backends.GitCmd
import           Control.Monad
import           Control.Monad.Identity
import           Data.List (sort)


data N n o (m :: * -> *) = N n (n -> m o)
exec :: N n o m -> m o
exec (N n f) = f n


dag :: HasGit m => N String (GitOutput String) m
dag = let branch = N "branch" $ git0            $ return ("World", "Hello ")
          left   = N "left"   $ git1 branch     $ return . fst
          right  = N "right"  $ git1 branch     $ return . snd
      in           N "join"   $ git2 left right $ return . uncurry (++)


demo :: IO ()
demo = withGit "repo" $ exec dag >>= print


git0 :: (HasGit m, Read o, Show o) => m o -> FilePath -> m (GitOutput o)
git0 job path = do
    gitCheckRunEffect path [] job
    gitReadOutput path


git1 :: (HasGit m, Read o, Show o) => N n (GitOutput a) m
                                   -> (a -> m o)
                                   -> FilePath
                                   -> m (GitOutput o)
git1 input job path = do
    (GitOutput depHead body) <- exec input
    gitCheckRunEffect path [depHead] (job body)
    gitReadOutput path


git2 :: (HasGit m, Read o, Show o) => N n (GitOutput a) m
                                   -> N n (GitOutput b) m
                                   -> ((a, b) -> m o)
                                   -> FilePath
                                   -> m (GitOutput o)
git2 input1 input2 job path = do
    (GitOutput depHead1 body1) <- exec input1
    (GitOutput depHead2 body2) <- exec input2
    gitCheckRunEffect path [depHead1, depHead2] (job (body1, body2))
    gitReadOutput path


gitCheckRunEffect :: (HasGit m, Show o) => FilePath -> [GitHeader] -> m o -> m ()
gitCheckRunEffect path inputHeaders effect = do
    let newMsg = sort inputHeaders -- kind of important
    exists <- gitExists path
    go <- if exists
        then do oldMsg <- read <$> gitMessage path
                return $ oldMsg /= newMsg
        else return True
    let commit = gitCommit path (show newMsg)
    when go $ effect >>= commit . show
