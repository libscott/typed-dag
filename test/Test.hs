{-# LANGUAGE QuasiQuotes #-}


import           Control.Dag
import           Control.Monad.IO.Class
import           Data.List (sort)
import           System.Environment



-- dag functions should output a terminating node which inputs all the leaf nodes.
-- that way the map can indeed be pure, since the leaf node will include everything
-- and nodes will have knowledge of their parents.


type UserId = Int
type Name = String
type GroupId = Int


groups :: [(GroupId, Name)]
groups = [ (1, "bigmoon"   )
         , (2, "bigspoon"  )
         ]


dag :: App m => GitNode m (Int, Int)
dag = let inputMessages  = in0out1 "import" "parse"                                 (fileInput "inputs/haskellirc.txt") >> [codeHash| return . lines |]
          (msgs, metas)  = in1out2 "import" "categorize"        inputMessages       [codeHash| return . lines |]
      in                   in1out1 "export" "nIrcEvents"        splitMessags


main :: IO ()
main = do
    args <- getArgs
    debugLogging
    runApp (head args) "" $ do
        execute dag
        playNode $ terminate dag
