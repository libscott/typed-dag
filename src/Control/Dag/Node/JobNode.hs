module Control.Dag.Node.JobNode
    ( JobArgs (..)
    ) where


-- (base path, input paths, completion callback)
data JobArgs i = JobArgs FilePath [FilePath] (FilePath -> IO ())
