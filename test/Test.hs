{-# LANGUAGE QuasiQuotes #-}


import           Control.Applicative
import           Control.Dag
import           Control.Monad.IO.Class
import           System.Environment


-- 2 features: optional outputs (don't commit)
--

userInput :: HasGit m => GitNode m String
userInput = in0out1 "userinput"
    [codeHash| liftIO $ putStr "Enter a sentence: " >> getLine
             |]


dag :: HasGit m => (GitNode m String, GitNode m String)
dag = let (l, r) = in1out2 "classify"
    [codeHash| \str -> if isUpper (head str) then ]



main :: IO ()
main = do
    args <- getArgs
    debugLogging
    withGit (head args) $ do
        let (l, r) = dag
        out <- (,) <$> execute l <*> execute r
        print out
