{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}



data OutputProxy2 n oa ob (m :: * -> *) = OutputProxy2 n ((oa -> m ()) -> m ())




class (Functor m, Monad m) => PullNode1 n o (m :: * -> *)
  where
      pull1 :: n -> n
      pull1 = id
      execute1 :: n -> (o -> m ()) -> m ()


-- possibility to use polymorphic container for ouptut and single callback rather than multiple output callbacks
class (Functor m, Monad m) => PullNode2 n a b (m :: * -> *)
  where
    pull2 :: n -> ( OutputProxy2 n oa ob m
                  , OutputProxy2 n ob oa m
                  )
    pull2 node = ( OutputProxy2 (\f -> execute2 node (\(ma, mb) -> maybe (return ()) f ma))
                 , OutputProxy2 (\f -> execute2 node (\(ma, mb) -> maybe (return ()) f mb))
                 )
    execute2 :: n -> ((Maybe a, Maybe b) -> m ()) -> m ()



instance (Functor m, Monad m) => PullNode1 (OutputProxy2 oa m) oa m where
    execute1 (OutputProxy2 f) = f


data EvenOdd u = EvenOdd u


instance PullNode1 u Int IO => PullNode2 (EvenOdd u) Int String IO where
    execute2 (EvenOdd u) cb = execute1 u $
        \n -> cb (if even n then (Just n, Nothing) else (Nothing, Just (show n)))


main :: IO ()
main = return ()
