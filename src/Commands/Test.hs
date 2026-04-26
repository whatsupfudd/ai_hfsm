module Commands.Test where

import Data.Text (Text)
import qualified Data.Text as T

import Options.Runtime (RunOptions)
import Options.Cli (TestOpts (..))



testCmd :: TestOpts -> RunOptions -> IO ()
testCmd testOpts rtOpts =
  putStrLn $ "@[testCmd] starting test: " <> T.unpack testOpts.testName