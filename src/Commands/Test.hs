module Commands.Test where

import Data.Text (Text)
import qualified Data.Text as T

import Options.Runtime (RunOptions(..))
import Options.Cli (TestOpts (..))

import Demo.ApprovalRuntime (testA)
import Demo.CommitTurnRegression (regression)
import qualified Demo.DistributedSafetyRegression as Distributed
import qualified Demo.DistributedPostgresRegression as DistributedPg


testCmd :: TestOpts -> RunOptions -> IO ()
testCmd testOpts rtOpts = do
  putStrLn $ "@[testCmd] starting test: " <> T.unpack testOpts.testName
  case testOpts.testName of
    "approval" -> testA testOpts rtOpts
    "commitTurn" -> regression
    "distributed" -> Distributed.regression
    "distributedDb" -> DistributedPg.regressionPg rtOpts.pgDbConf
    _ -> putStrLn $ "@[testCmd] unknown test: " <> T.unpack testOpts.testName
