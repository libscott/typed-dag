{-# LANGUAGE QuasiQuotes #-}


import           Control.Dag
import           System.Environment




dag :: HasGit m =>  GitNode m String
dag = gitNode0 "a" $ [codeHash| return "Hello world" |]


main :: IO ()
main = do
    args <- getArgs
    debugLogging
    withGit (head args) (execute dag >>= print)
