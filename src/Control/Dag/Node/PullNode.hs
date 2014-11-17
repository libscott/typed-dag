{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}



data Pull n u = Pull n u


pull0 n = Pull n ()
pull1 n a = Pull n a
pull2 n a b = Pull n (a, b)
pull3 n a b c = Pull n (a, (b, c))


class PullNode o p m where
    pull :: p -> m o

--
-- instance (Functor m, Monad m) => PullNode ([Int -> m ()], [String -> m ()]) MyMultiOutputNode m where
--     pull _ (cb1s, cb2s) = mapM_ ($1) cb1s >> mapM_ ($"yay") cb2s



instance PullNode o () m => PullNode o (Pull (() -> m o) ()) m where
    pull (Pull node upstream) = undefined -- says do this and call this guy when done.


instance PullNode i p m => PullNode o (i -> m o, p) m where
    pull (Pull node upstream) =  (pull a)


instance (PullNode i p m, PullNode j q m) => PullNode (i, j) (p, q) m where
    pull (Pull node (a, b)) = (pull a, pull b)


-- git is an uninterrupting wrapper which allows resumption


main :: IO ()
main = do
    let top () = 1::Int
    let middle1 = (show :: Int -> String, top)
    let middle2 = (show :: Int -> String, top)
    let bottom = ((=="1"), (middle1, middle2))
    print (pull bottom)
