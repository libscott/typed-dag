{-# LANGUAGE QuasiQuotes #-}


import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 as C8 (lines)
import qualified Data.Conduit.List as CL
import           Control.Monad.IO.Class
import           Data.List (sort)
import           System.Environment

import Control.Dag
import Control.Dag.Prelude



-- A 2 node graph
-- Like Conduit but it's a DAG

dag :: App m => GitNode m ByteString
dag = let haskellirc = watchedFile "inputs/haskellirc.txt" in haskellirc
      in               in1out1 "outputs" "lines" haskellirc [codeHash| return . ($= CL.map C8.lines) |]


main :: IO ()
main = do
    args <- getArgs
    debugLogging
    runApp (head args) "" $ do
        execute dag
        -- playNode $ terminate dag
        return ()
