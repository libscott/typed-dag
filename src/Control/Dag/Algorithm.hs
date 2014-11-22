module Control.Dag.Algorithm
    ( Algorithm (..)
    , codeHash
    , fileInput
    , script
    ) where


import           Control.Applicative
import           Control.Monad.IO.Class
import           Crypto.Hash (SHA1)
import           Crypto.Hash.Types
import           Crypto.Hash.Conduit
import           Data.ByteString as BS
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Hashable
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.Meta.Parse (parseExp)
import           System.IO
import           System.Process
import           Text.Printf

import           Control.Dag.Types
import           Control.Dag.Utils


embed :: String -> Q Exp
embed expr = return gotExp
  where
    gotExp = either error id $ parseExp expr


toStringAlgo :: String -> Q Exp
toStringAlgo expr = embed $ printf "Algorithm (%v) (return %v)" expr (hash expr)


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


fileInput :: App m => FilePath -> Algorithm (Source m ByteString) m
fileInput path = Algorithm (sourceFile path) (show <$> sha1file path)


script :: App m => FilePath -> [String] -> Algorithm (ConduitM ByteString ByteString m ()) m
script path args = Algorithm scriptConduit (show <$> sha1file path)
  where
    procArgs = (proc path args) { std_in = CreatePipe
                                , std_out = CreatePipe
                                }
    -- scriptConduit :: App m => Conduit String m String
    scriptConduit = do
        minput <- await
        case minput of
            Nothing    -> return ()
            Just input -> do
                out <- liftIO $! do
                    (Just hin, Just hout, _, p) <- createProcess procArgs
                    BS.hPutStr hin input
                    hClose hin
                    rc <- waitForProcess p
                    checkRc (path:args) (return ()) rc
                    BS.hGetContents hout
                yield out


sha1file :: MonadIO m => FilePath -> m (Digest SHA1)
sha1file = hashFile
