{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}



data OutputProxy2 oa ob (m :: * -> *) = OutputProxy ((oa -> m ()) -> m ())



class (Functor m, Monad m) => PullNode1 n o (m :: * -> *)
  where
      pull1 :: n -> n
      pull1 = id
      execute1 :: n -> (o -> m ()) -> m ()


class (Functor m, Monad m) => PullNode2 n oa ob (m :: * -> *)
  where
    pull2 :: n -> ( OutputProxy2 oa ob m
                  , OutputProxy2 ob oa m
                  )
    pull2 node = ( OutputProxy (\f -> execute2 node (f, \_ -> return ()))
                 , OutputProxy (\f -> execute2 node (\_ -> return (), f))
                 )
    execute2 :: n -> (oa -> m (), ob -> m ()) -> m ()



instance (Functor m, Monad m) => PullNode1 (OutputProxy2 oa ob m) oa m where
    execute1 (OutputProxy f) = f


data EvenOdd u = EvenOdd u


instance PullNode1 u Int IO => PullNode2 (EvenOdd u) Int String IO where


    execute2 (EvenOdd u) (cb1, cb2) = execute1 u $
        \n -> if even n then cb1 n else cb2 (show n)


main :: IO ()
main = return ()
