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



toStringAlgo :: String -> Q Exp
toStringAlgo expr = do
    let str = printf "Algorithm (%v) (return \"%v\")" expr (hash expr)
    return $ either error id $ parseExp (str::String)


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


fileInput :: App m => FilePath -> Algorithm (() -> Source m ByteString) m
fileInput path = Algorithm (\() -> sourceFile path) (show <$> sha1file path)


script :: App m => FilePath -> [String] -> Algorithm (ConduitM ByteString ByteString m ()) m
script path args = Algorithm scriptConduit (show <$> sha1file path)
  where
    procArgs = (proc path args) { std_in = CreatePipe
                                , std_out = CreatePipe
                                }
    scriptConduit :: App m => Conduit ByteString m ByteString
    scriptConduit = do
        minput <- await
        case minput of
            Nothing    -> return ()
            Just input -> do
                hout <- liftIO $! do
                    (Just hin, Just hout, _, p) <- createProcess procArgs
                    BS.hPutStr hin input
                    hClose hin
                    rc <- waitForProcess p
                    checkRc (path:args) (return hout) rc
                sourceHandle hout
                liftIO $ hClose hout


sha1file :: MonadIO m => FilePath -> m (Digest SHA1)
sha1file = hashFile
