{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , RankNTypes
  , TypeSynonymInstances
  #-}


module Control.Dag.Node.ContNode
    ( await
    , Cont (Ready)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.State.Strict
import           Data.Dynamic

import           Control.Dag.Node.StateNode
import           Control.Dag.Types.Node


data Cont a m r = Ready   (Cont a m r)
                | Waiting (a -> Cont a m r)
                | M       (m (Cont a m r))
                | Pure    r
    deriving (Typeable)


instance (Functor m, Monad m, Typeable r, Typeable a, Typeable m) => TypeableNode (Cont a m r) a m


instance Monad m => Monad (Cont a m) where
    return = Pure
    (>>=)  = flip bind


instance (Functor m, Monad m) =>
         Node (Cont a m r) a m
  where
    fold cont a = do
        cont' <- runCont cont
        case cont' of
            Waiting f -> runCont (f a)
            Pure b    -> return $ Pure b
            _         -> error "can't happen because `runcont`"


instance MonadTrans (Cont a) where
    lift m = M (liftM Pure m)


instance Monad m => Functor (Cont a m) where
    fmap f = bind (return . f)


instance Monad m => Applicative (Cont a m) where
    pure    = return
    a <*> b = bind (`fmap` b) a
    (*>) = (>>)


instance MonadIO m => MonadIO (Cont a m) where
    liftIO = lift . liftIO


instance MonadState DagState m => MonadState DagState (Cont a m) where
    get = lift get
    put = lift . put


runCont :: Monad m => Cont t m t1 -> m (Cont t m t1)
runCont cont = case cont of
    Ready p -> runCont p
    M m     -> m >>= runCont
    c       -> return c


bind :: Monad m => (r -> Cont a m r') -> Cont a m r -> Cont a m r'
bind f s = case s of
    Ready   p -> Ready $ p >>= f
    Waiting p -> Waiting $ p >=> f
    M       p -> M       $ liftM (>>= f) p
    Pure    r -> f r


await :: Monad m => Cont a m a
await = Waiting return
