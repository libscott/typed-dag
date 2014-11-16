{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}


module Control.Dag.Node.EmitterNode
    ( Emitter (..)
    , makeEmitter
    , subscribe
    ) where


import           Control.Lens
import           Control.Monad.State.Strict

import           Control.Dag.Types.Node


-- | An emitter knows how to access it's subscribers from state
data Emitter i s m = Emitter { addSubscriber :: (i -> m ()) -> s -> s
                             , getSubscribers :: s -> [i -> m ()]
                             }


instance (Functor m, Monad m, MonadState s m) => Node i (Emitter i s m) m where
    send (Emitter _ getSubs) input = get >>= mapM_ ($input) . getSubs



subscribe :: (Monad m, Node i n m, MonadState s m)
          => n -> Emitter i s m -> m ()
subscribe node em = modify $ addSubscriber em $ send node


makeEmitter :: Monad m => ALens s s [i -> m ()] [i -> m ()] -> m (Emitter i s m)
makeEmitter lenz = return $ Emitter ((#%~) lenz . (:)) (^# lenz)
