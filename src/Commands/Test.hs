-- | CT-05: existing commands preserved; adds history/historyDb regressions.
module Commands.Test where

import qualified Data.Text as T
import Options.Runtime (RunOptions(..))
import Options.Cli (TestOpts(..))

import Demo.ApprovalRuntime (testA)
import Demo.CommitTurnRegression (regression)
import qualified Demo.DistributedSafetyRegression as Distributed
import qualified Demo.DistributedPostgresRegression as DistributedPg
import qualified Demo.ActivityRegression as Activity
import qualified Demo.HistoryRegression as History

testCmd :: TestOpts -> RunOptions -> IO ()
testCmd testOpts rtOpts = do
  putStrLn $ "@[testCmd] starting test: " <> T.unpack testOpts.testName
  case testOpts.testName of
    "approval" -> testA testOpts rtOpts
    "commitTurn" -> regression
    "distributed" -> Distributed.regression
    "distributedDb" -> DistributedPg.regressionPg rtOpts.pgDbConf
    "activity" -> Activity.regression
    "activityDb" -> Activity.regressionPg rtOpts.pgDbConf
    "history" -> History.regression
    "historyDb" -> History.regressionPg rtOpts.pgDbConf
    _ -> putStrLn $ "@[testCmd] unknown test: " <> T.unpack testOpts.testName
