module Commands.Test where

import Data.Text (Text)
import qualified Data.Text as T

import Options.Runtime (RunOptions)
import Options.Cli (TestOpts (..))

import Demo.ApprovalRuntime (testA)


testCmd :: TestOpts -> RunOptions -> IO ()
testCmd testOpts rtOpts = do
  putStrLn $ "@[testCmd] starting test: " <> T.unpack testOpts.testName
  case testOpts.testName of
    "approval" -> testA
    _ -> putStrLn $ "@[testCmd] unknown test: " <> T.unpack testOpts.testName