{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}


import Control.Monad.Writer

data OutputProxy o u = OutputProxy u l




class (Functor m, Monad m) => PullNode n o (m :: * -> *)
  where
      execute :: n -> i -> m ()
      pull :: n -> u -> b


instance (PullNode n (a, b) m) => PullNode (Pull OutputProxy u) a m where
    execute (Pull OutputProxy upstream) callback = do
        out <- execWriterT $ execute upstream tell
        callback out






data Pull node upstreams = Pull node upstreams

--
-- data MyComplexNode = MyComplexNode
-- instance PullNode n (a, b) m where
--     --pull n = (OutputProxy )
--     execute n upstreams = do




-- (o3, o4) = pull node (o1, o2)
