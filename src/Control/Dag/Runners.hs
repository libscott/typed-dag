module Control.Dag.Runners where


import Control.Dag.Prelude
import Control.Dag.Play
import Control.Dag.Types
import Control.Dag.Utils


type Committer o m      = InputVersions -> o -> m ()
type GitNode2 m a b     = (GitNode m a, GitNode m b)
type GitNode3 m a b c   = (GitNode m a, GitNode m b, GitNode m c)
type Runner m o         = Committer o m -> m ()
type Source2 m a b      = (Source m a, Source m b)
type Source3 m a b c    = (Source m a, Source m b, Source m c)

------------------------------------------------------------------
-- | Runner
------------------------------------------------------------------


-- | Get outputs from upstream, compile and compare version
--   information, then run (or dont run)
runner :: App m
       => FilePath
       -> m ([InputHeader], i)
       -> Algorithm (i -> m o) m
       -> Runner m o
runner path getInputs (Algorithm f getAlgoVer) commit = do
    algoVer <- getAlgoVer
    (inputHeaders, inputs) <- getInputs
    let newVers = InputVersions algoVer inputHeaders
        effect = f inputs >>= commit newVers
    result <- compareInputs path newVers
    case result of
        Same -> info1 "%v has not changed" path
        Changed -> do
            info1 "%v has new inputs, running" path
            effect
        DoesNotExist -> do
            info1 "%v does not exist, running" path
            effect
