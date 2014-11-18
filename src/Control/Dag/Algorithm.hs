module Control.Dag.Algorithm where

import Data.Hashable
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Printf
import Language.Haskell.Meta.Parse (parseExp)


data AlgoVersion = CodeHash Int
    deriving (Show, Eq)


data Algo f = Algo f AlgoVersion


embed :: String -> Q Exp
embed expr = return gotExp
  where
    gotExp = either error id $ parseExp expr


toStringAlgo :: String -> Q Exp
toStringAlgo expr = embed $ printf "Algo (%v) (CodeHash (%v))" expr (hash expr)


codeHash :: QuasiQuoter
codeHash = QuasiQuoter
    { quotePat  = error "Control.Dag.Algorithm:\
                      \ quasiquoter used in pattern context"
    , quoteType = error "Control.Dag.Algorithm:\
                      \ quasiquoter used in type context"
    , quoteExp  = toStringAlgo
    , quoteDec  = error "Control.Dag.Algorithm:\
                      \ quasiquoter used in declaration context"
}
