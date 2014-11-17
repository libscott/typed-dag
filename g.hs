{-# LANGUAGE OverloadedStrings           #-}

import Control.Applicative
import Control.Monad
import Git
import Git.Reference
import Git.Libgit2
import Git.Object
import Git.Types
import Control.Monad.IO.Class



main :: IO ()
main = withRepository lgFactory "." $ do
    moid <- resolveReference "HEAD"
    case moid of
        Nothing -> return ()
        Just oid -> do
            CommitObj headCommit <- lookupObject oid
            objs <- listAllObjects Nothing (commitOid headCommit)
            forM_ objs $ \obj -> case obj of
                BlobObjOid o -> liftIO $ print o
                _ -> return ()
                -- TreeObjOid o -> liftIO $ print o
                -- CommitObjOid o -> liftIO $ print o
                -- TagObjOid o -> liftIO $ print o
                -- BlobObjOid o -> liftIO $ print o

    -- can haz path lookup??
