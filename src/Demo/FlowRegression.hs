{-# LANGUAGE NoFieldSelectors #-}
-- | DF-01 pure validation and dataflow topology regression.
module Demo.FlowRegression (regression, diamondSpec) where

import Control.Monad (unless)
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Hfsm.Flow.Spec
  ( FlowSpec(..), TaskDef(..), EdgeDef(..)
  , validateFlowSpec, topologicalOrder, incomingEdges )

assert :: Bool -> String -> IO ()
assert yes message = unless yes (fail message)

task :: T.Text -> [T.Text] -> [T.Text] -> TaskDef
task key inputs outputs = TaskDef
  { key=key,executor="fixture",inputs=inputs,outputs=outputs
  , cpu=1,memoryMb=64,capabilities=[],maxAttempts=3
  , timeoutSeconds=5,priority=0 }

diamondSpec :: FlowSpec
diamondSpec = FlowSpec
  { tasks = [task "A" [] ["a"],task "B" ["a"] ["b"],
             task "C" ["a"] ["c"],task "D" ["b","c"] ["result"]]
  , edges = [EdgeDef "A" "a" "B" "a", EdgeDef "A" "a" "C" "a",
             EdgeDef "B" "b" "D" "b", EdgeDef "C" "c" "D" "c"] }

regression :: IO ()
regression = do
  assert (validateFlowSpec diamondSpec == Right ()) "diamond should validate"
  assert (topologicalOrder diamondSpec == Right ["A","B","C","D"])
    "deterministic topological order"
  assert (length (incomingEdges "D" diamondSpec) == 2) "D must have two input edges"
  assert (decode (encode diamondSpec) == Just diamondSpec) "JSON codec round trip"
  let cycleGraph = diamondSpec { edges=diamondSpec.edges <>
        [EdgeDef "D" "result" "A" "cycle"]
        , tasks = map (\t -> if t.key=="A" then t { inputs=["cycle"] } else t) diamondSpec.tasks }
  assert (either (const True) (const False) (validateFlowSpec cycleGraph))
    "directed cycle was not rejected"
  let missing = diamondSpec { edges=take 3 diamondSpec.edges }
  assert (either (const True) (const False) (validateFlowSpec missing))
    "unconnected declared port was accepted"
  let duplicate = diamondSpec { edges=diamondSpec.edges <> [head diamondSpec.edges] }
  assert (either (const True) (const False) (validateFlowSpec duplicate))
    "duplicate edge was accepted"
  let badResources = diamondSpec { tasks = map (\t -> if t.key=="B" then t { cpu=0 } else t)
                                            diamondSpec.tasks }
  assert (either (const True) (const False) (validateFlowSpec badResources))
    "zero CPU demand was accepted"
  putStrLn "FlowRegression (pure): PASS"
