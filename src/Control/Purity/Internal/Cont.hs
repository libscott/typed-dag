{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , RankNTypes
  , TypeSynonymInstances
  #-}


module Control.Purity.Internal.Cont
    ( addCont
    , await,
      Cont(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.State.Strict
import           Data.Dynamic

import           Control.Purity.Internal.Dag


data Cont a m r = Ready   (Cont a m r)
                | Waiting (a -> Cont a m r)
                | M       (m (Cont a m r))
                | Pure    r
    deriving (Typeable)


instance Monad m => Monad (Cont a m) where
    return = Pure
    (>>=)  = flip bind


instance (Functor m, Monad m, Typeable r, Typeable a, Typeable m, Show a) =>
         Node (Cont a (Dag m) r) a m
  where
    send' cont a = do
        cont' <- runCont cont
        case cont' of
            Waiting f -> runCont (f a)
            Pure b    -> return $ Pure b
            c         -> error $ "Can't send: " ++ show c


instance MonadTrans (Cont a) where
    lift m = M (liftM Pure m)


instance Monad m => Functor (Cont a m) where
    fmap f = bind (return . f)


instance Monad m => Applicative (Cont a m) where
    pure    = return
    a <*> b = bind (`fmap` b) a
    (*>) = (>>)


instance (Typeable a, Typeable m, Typeable r) => Show (Cont a m r) where
    show a = case a of
        Ready n   -> "Ready" ++ show n
        Waiting _ -> "Waiting"
        M _       -> "M"
        Pure b    -> "Pure" ++ show (toDyn b)


instance MonadIO m => MonadIO (Cont a m) where
    liftIO = lift . liftIO


instance MonadState DagState m => MonadState DagState (Cont a m) where
    get = lift get
    put = lift . put


runCont :: Monad m => Cont t m t1 -> m (Cont t m t1)
runCont (Ready p) = runCont p
runCont (M m)     = m >>= runCont
runCont c         = return c


bind :: Monad m => (r -> Cont a m r') -> Cont a m r -> Cont a m r'
bind f s = case s of
    Ready   p -> Ready $ p >>= f
    Waiting p -> Waiting $ p >=> f
    M       p -> M       $ liftM (>>= f) p
    Pure    r -> f r


addCont :: (Node (Cont a (Dag m) r) a m, Typeable a, Typeable r, Typeable m) => Cont a (Dag m) r -> Dag m (NodeId (Cont a (Dag m) r) a)
addCont = addNode . Ready


await :: Monad m => Cont a m a
await = Waiting return
