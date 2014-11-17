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


data N n o m = N n (n -> m o)
exec :: N n o m -> m o
exec (N n f) = f n


dag :: (Applicative m, Functor m, Monad m) => N () String m
dag = let n2in = N () (\_ -> return ("World", "Hello "))
          n1left = N () (\_ -> fst <$> exec n2in)
          n1right = N () (\_ -> snd <$> exec n2in)
      in N () (\_ -> (++) <$> exec n1right <*> exec n1left)


demo :: IO ()
demo = exec dag >>= print
