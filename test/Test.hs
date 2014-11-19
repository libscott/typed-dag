{-# LANGUAGE QuasiQuotes #-}


import           Control.Applicative
import           Control.Dag
import           System.Environment



top :: Monad m => Algo (m (String, String))
top = [codeHash| return ("b", "a") |]


-- makes no sense because stores outputs in same file.
-- need to figure out how to separate outputs.
-- job needs to be able to emit outputs with a lens.
-- The emissions should be typed too.
-- immediate problem to be solved is outputting a tuple
-- of outputs during compilation.

-- let (o1, o2) = gitNode0 ...
-- the outputs are simply proxies which trigger the run of the parent and
-- return the individual output that we're interested in

-- this is going to be easy, once we figure out:
-- curateOutputs :: Many o => GitNode m o -> (output0, output1 ... )
-- first thing is to write the output commiter.


dag :: HasGit m => (GitNode m String, GitNode m String)
dag = gitNode0_2 "top" ("left", "right") $ gitNode0 "top" top


main :: IO ()
main = do
    args <- getArgs
    debugLogging
    withGit (head args) $ do
        let (l, r) = dag
        out <- (,) <$> execute l <*> execute r
        print out
