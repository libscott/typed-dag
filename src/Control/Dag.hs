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
import           Data.List (sort)


data N n o (m :: * -> *) = N n (n -> m o)
exec :: N n o m -> m o
exec (N n f) = f n


dag :: (Applicative m, Functor m, Monad m) => N () String m
dag = let n2in = N () (\_ -> return ("World", "Hello "))
          n1left = N () (\_ -> fst <$> exec n2in)
          n1right = N () (\_ -> snd <$> exec n2in)
      in N () (\_ -> (++) <$> exec n1right <*> exec n1left)


demo :: IO ()
demo = withGit "repo" $ exec dag >>= print


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mcond effect = mcond >>= flip unless effect


git0 :: (HasGit m, Read o, Show o) => FilePath -> m () -> m (GitOutput o)
git0 path job = do
    gitCheckRunEffect path [] job
    gitReadOutput path


git1 :: (HasGit m, Read o, Show o) => FilePath
                                   -> N n (GitOutput a) m
                                   -> (a -> m o)
                                   -> m (GitOutput o)
git1 path input job = do
    (GitOutput depHead body) <- exec input
    gitCheckRunEffect path [depHead] (job body)
    gitReadOutput path


git2 :: (HasGit m, Read o, Show o) => FilePath
                                   -> N n (GitOutput a) m
                                   -> N n (GitOutput b) m
                                   -> (a -> b -> m o)
                                   -> m (GitOutput o)
git2 path input1 input2 job = do
    (GitOutput depHead1 body1) <- exec input1
    (GitOutput depHead2 body2) <- exec input2
    gitCheckRunEffect path [depHead1, depHead2] (job body1 body2)
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
