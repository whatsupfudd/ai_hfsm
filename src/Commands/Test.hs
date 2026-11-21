-- | DF-02 test dispatcher: complete replacement, preserving CT-01..CT-06
-- and DF-01 regression names without modifying any existing test modules.
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
import qualified Demo.OperationsRegression as Operations
import qualified Demo.OperationsChaosRegression as Chaos
import qualified Demo.OperationsChaosProcess as ChaosProcess
import qualified Demo.FlowRegression as Flow
import qualified Demo.FlowPostgresRegression as FlowPg
import qualified Demo.FlowDynamicRegression as Dynamic
import qualified Demo.FlowDynamicPostgresRegression as DynamicPg

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
    "operations" -> Operations.regression
    "operationsDb" -> Operations.regressionPg rtOpts.pgDbConf
    "chaosDb" -> Chaos.regressionPg rtOpts.pgDbConf
    "chaosFixtureDb" -> ChaosProcess.createFixtureDb rtOpts.pgDbConf
    "chaosWorkerDb" -> ChaosProcess.chaosWorkerDb rtOpts.pgDbConf
    "chaosInspectDb" -> ChaosProcess.inspectFixtureDb rtOpts.pgDbConf
    "flow" -> Flow.regression
    "flowDb" -> FlowPg.regressionPg rtOpts.pgDbConf
    "flowDynamic" -> Dynamic.regression
    "flowDynamicDb" -> DynamicPg.regressionPg rtOpts.pgDbConf
    _ -> putStrLn $ "@[testCmd] unknown test: " <> T.unpack testOpts.testName
