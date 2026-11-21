{-# LANGUAGE NoFieldSelectors #-}

-- | Pure DF-02 bounded expansion, stable ordering and malformed-manifest tests.
module Demo.FlowDynamicRegression (regression, exampleScatter) where

import Control.Monad (unless)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import Hfsm.Flow.Dynamic.Spec
  ( ResourcePolicy(..), PartitionDef(..), ScatterPlan(..)
  , PlannedExpansion(..), planExpansion, partitionTaskKey, reductionTaskKey )
import Hfsm.Flow.Spec (EdgeDef(..), TaskDef(..))

assert :: Bool -> String -> IO ()
assert condition message = unless condition (fail message)

basicPolicy :: ResourcePolicy
basicPolicy = ResourcePolicy
  { cpu=1,memoryMb=64,capabilities=[],maxAttempts=3
  , timeoutSeconds=30,priority=0 }

exampleScatter :: [(T.Text,UUID)] -> Int -> ScatterPlan
exampleScatter values width = ScatterPlan
  { groupKey="example", partitions=[PartitionDef key artifact | (key,artifact) <- values]
  , fanIn=width,partitionExecutor="df2.partition",aggregateExecutor="df2.reduce"
  , partitionProfile=basicPolicy,aggregateProfile=basicPolicy }

regression :: IO ()
regression = do
  let values=[("p"<>T.pack (show index),U.nil) | index<-[1..5::Int]]
      expanded=planExpansion (exampleScatter values 2)
  graph <- either (fail . show) pure expanded
  assert (length graph.tasks==11) "5 leaves / fan-in 2 requires 11 tasks"
  assert (length graph.edges==10) "reduction tree requires 10 edges"
  assert (graph.rootTask==reductionTaskKey "example" 2 1)
    "wrong deterministic root of three aggregation levels"
  assert (map (\(key,_,_) -> key) graph.bindings==
          map (partitionTaskKey "example") [1..5])
    "partition manifest order was not preserved"
  let firstLevel = filter (\edge -> edge.toTask==reductionTaskKey "example" 0 1) graph.edges
  assert (map (.toPort) firstLevel==["i000001","i000002"])
    "gather input port order must follow manifest order"
  let stable = planExpansion (exampleScatter values 2)
  assert (stable==expanded) "graph expansion not deterministic"

  zero <- either (fail . show) pure (planExpansion (exampleScatter [] 8))
  assert (length zero.tasks==1 && null zero.edges &&
          zero.rootTask==reductionTaskKey "example" 0 1)
    "zero-partition reduction must have one ready identity node"
  one <- either (fail . show) pure
    (planExpansion (exampleScatter [("one",U.nil)] 8))
  assert (length one.tasks==2 && length one.edges==1)
    "singleton must still pass through one reducer"

  large <- either (fail . show) pure
    (planExpansion (exampleScatter
       [("p"<>T.pack (show index),U.nil) | index<-[1..512::Int]] 64))
  assert (length large.tasks==521) "512 partition aggregation count"
  assert (length large.edges==520) "512 partition aggregation edge count"

  let duplicate=exampleScatter [("same",U.nil),("same",U.nil)] 2
  assert (case planExpansion duplicate of Left _ -> True; _ -> False)
    "duplicate partition names accepted"
  let tooWide=exampleScatter values 65
  assert (case planExpansion tooWide of Left _ -> True; _ -> False)
    "fan-in above 64 accepted"
  let badGroup=(exampleScatter values 2){groupKey="has spaces"}
  assert (case planExpansion badGroup of Left _ -> True; _ -> False)
    "invalid SQL task-key fragment accepted"
  putStrLn "FlowDynamicRegression: PASS"
