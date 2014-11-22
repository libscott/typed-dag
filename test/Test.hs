{-# LANGUAGE QuasiQuotes #-}


import           Data.ByteString (ByteString)
import           Control.Monad.IO.Class
import           Data.List (sort)
import           System.Environment

import Control.Dag



dag :: App m => GitNode m ByteString
dag = let inputMessages  = in0out1 "import"     "parse"                                 (fileInput "inputs/haskellirc.txt")
      in inputMessages
    --       (msgs, metas)  = in1out2 "categorize" ("msgs", "metas")         inputMessages       [codeHash| return . lines |]
    --   in                   in1out1 "export"     "nIrcEvents"        msgs


main :: IO ()
main = do
    args <- getArgs
    debugLogging
    runApp (head args) "" $ do
        execute dag
        -- playNode $ terminate dag
        return ()
