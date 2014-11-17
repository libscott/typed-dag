{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FunctionalDependencies #-}




-- class BiEmitter o a b where
--     emit2 :: o -> (a, b)
--
-- data HasTwo a b = HasTwo a b
-- instance BiEmitter (HasTwo a b) a b where
--     emit2 (HasTwo a b) = (a, b)
--
-- class UniEmitter o a where
--     emit1 :: o -> a
--
-- data LeftProxy n = LeftProxy n
--
-- instance UniEmitter (LeftProxy a) a where
--     emit1 LeftProxy


-- these are your new friends.


data OutputProxy2 n oa ob (m :: * -> *) = OutputProxy2 n



class (Functor m, Monad m) => PullNode1 n o (m :: * -> *) | n -> o
  where
      pull1 :: n -> n
      pull1 = id
      execute1 :: n -> (o -> m ()) -> m ()


class (Functor m, Monad m) => PullNode2 n oa ob (m :: * -> *) | n -> oa, n -> ob
  where
    pull2 :: n -> (OutputProxy2 n oa ob m, OutputProxy2 n ob oa m)
    pull2 node = (OutputProxy2 node, OutputProxy2 node)
    execute2 :: n -> (oa -> m (), ob -> m ()) -> m ()


instance (Functor m, Monad m, PullNode2 n oa ob m) =>
         PullNode1 (OutputProxy2 n oa ob m) oa m where
    execute1 (OutputProxy2 n) f = execute2 n (f, \_ -> return ())


instance (Functor m, Monad m, PullNode2 n oa ob m) =>
         PullNode1 (OutputProxy2 n oa ob m) ob m where
    execute1 (OutputProxy2 n) f = execute2 n (\_ -> return (), f)


data EvenOdd u = EvenOdd u


instance PullNode1 u Int IO => PullNode2 (EvenOdd u) Int String IO where
    execute2 (EvenOdd u) (cb1, cb2) = execute1 u $
        \n -> if even n then cb1 n else cb2 (show n)


main :: IO ()
main = return ()
