{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | DF-02 bounded expansion specification. The runtime never accepts an
-- unbounded generator or a callback that mutates its DAG after expansion.
module Hfsm.Flow.Dynamic.Spec
  ( ResourcePolicy(..), PartitionDef(..), ScatterPlan(..)
  , ExpansionErr(..), PlannedExpansion(..), planExpansion
  , partitionTaskKey, reductionTaskKey
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hfsm.Flow.Spec (TaskDef(..), EdgeDef(..), FlowSpec(..), validateFlowSpec, topologicalOrder)

-- | Same finite resource/retry contract as a DF-01 TaskDef.
data ResourcePolicy = ResourcePolicy
  { cpu :: Int
  , memoryMb :: Int
  , capabilities :: [Text]
  , maxAttempts :: Int
  , timeoutSeconds :: Int
  , priority :: Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data PartitionDef = PartitionDef
  { key :: Text
  , artifactId :: UUID
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Partition order is semantically significant: leaf ports are assigned in
-- manifest order. Aggregation MUST be deterministic for the same ordered list.
data ScatterPlan = ScatterPlan
  { groupKey :: Text
  , partitions :: [PartitionDef]
  , fanIn :: Int
  , partitionExecutor :: Text
  , aggregateExecutor :: Text
  , partitionProfile :: ResourcePolicy
  , aggregateProfile :: ResourcePolicy
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ExpansionErr = ExpansionErr Text
  deriving stock (Eq, Show)

-- | Complete static shape of one bounded expansion. Partition artifact
-- bindings are additionally stored in immutable SQL rows; they are not graph
-- edges because the artifacts have already been published by the scatterer.
data PlannedExpansion = PlannedExpansion
  { tasks :: [TaskDef]
  , edges :: [EdgeDef]
  , rootTask :: Text
  , bindings :: [(Text, Text, UUID)] -- (task, partition key, CAS artifact)
  } deriving stock (Eq, Show)

allowedKey :: Text -> Bool
allowedKey value = not (T.null value) && T.all validChar value
 where
  validChar char =
    (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') ||
    (char >= '0' && char <= '9') || char `elem` ("_-.:" :: String)

unique :: Ord a => [a] -> Bool
unique values = S.size (S.fromList values) == length values

pad6 :: Int -> Text
pad6 number = T.justifyRight 6 '0' (T.pack (show number))

partitionTaskKey :: Text -> Int -> Text
partitionTaskKey groupName ordinal = "df2:" <> groupName <> ":p:" <> pad6 ordinal

reductionTaskKey :: Text -> Int -> Int -> Text
reductionTaskKey groupName depth ordinal =
  "df2:" <> groupName <> ":r:" <> T.pack (show depth) <> ":" <> pad6 ordinal

mkTask :: Text -> Text -> [Text] -> [Text] -> ResourcePolicy -> TaskDef
mkTask taskName executorName inputPorts outputPorts policy = TaskDef
  { key=taskName, executor=executorName, inputs=inputPorts, outputs=outputPorts
  , cpu=policy.cpu, memoryMb=policy.memoryMb, capabilities=policy.capabilities
  , maxAttempts=policy.maxAttempts, timeoutSeconds=policy.timeoutSeconds
  , priority=policy.priority }

planExpansion :: ScatterPlan -> Either ExpansionErr PlannedExpansion
planExpansion plan = do
  let partCount = length plan.partitions
      names = map (.key) plan.partitions
  unless (T.length plan.groupKey <= 24 && allowedKey plan.groupKey) $
    Left (ExpansionErr "group key must be 1..24 safe ASCII characters")
  unless (partCount <= 512 && all (\name -> T.length name <= 128 && allowedKey name) names && unique names) $
    Left (ExpansionErr "partitions must be unique and bounded to 512")
  unless (plan.fanIn >= 2 && plan.fanIn <= 64) $
    Left (ExpansionErr "aggregation fanIn must be 2..64")
  unless (T.length plan.partitionExecutor <= 128 && allowedKey plan.partitionExecutor &&
          T.length plan.aggregateExecutor <= 128 && allowedKey plan.aggregateExecutor &&
          plan.partitionExecutor /= "df2.scatter" &&
          plan.aggregateExecutor /= "df2.scatter") $
    Left (ExpansionErr "invalid partition/aggregate executor")
  validatePolicy plan.partitionProfile
  validatePolicy plan.aggregateProfile
  let partTasks = zipWith (\ordinal part ->
        mkTask (partitionTaskKey plan.groupKey ordinal) plan.partitionExecutor
               [] ["value"] plan.partitionProfile) [1..] plan.partitions
      partBindings = zipWith
        (\ordinal part -> (partitionTaskKey plan.groupKey ordinal,part.key,part.artifactId))
        [1..] plan.partitions
      (allReducers, allEdges, finalKey) = buildTree plan
      expanded = PlannedExpansion
        { tasks=partTasks <> allReducers, edges=allEdges
        , rootTask=finalKey, bindings=partBindings }
  -- Validate the generated DAG with the DF-01 validator, not a bespoke
  -- second cycle detector.
  let generated = FlowSpec expanded.tasks expanded.edges
  -- Registered DF-01 definitions are capped at 256 static tasks. Expansions
  -- have their own independently checked 4096-task cap, so for a larger tree
  -- we reuse the acyclicity check without incorrectly invoking that limit.
  let verified = if length expanded.tasks <= 256
                 then fmap (const ()) (validateFlowSpec generated)
                 else fmap (const ()) (topologicalOrder generated)
  case verified of
    Left err -> Left (ExpansionErr (T.pack (show err)))
    Right () -> Right expanded
 where
  validatePolicy :: ResourcePolicy -> Either ExpansionErr ()
  validatePolicy policy =
    unless (policy.cpu >= 1 && policy.cpu <= 1024 &&
            policy.memoryMb >= 1 && policy.memoryMb <= 1048576 &&
            policy.maxAttempts >= 1 && policy.maxAttempts <= 100 &&
            policy.timeoutSeconds >= 1 && policy.timeoutSeconds <= 86400 &&
            policy.priority >= (-20) && policy.priority <= 20 &&
            length policy.capabilities <= 32 && unique policy.capabilities &&
            all (\name -> T.length name <= 128 && allowedKey name) policy.capabilities) $
      Left (ExpansionErr "invalid resource policy")

-- | Levels form an ordered, deterministic reduction tree. Port i000001 is
-- always the earliest child in that reduction, regardless of completion time.
buildTree :: ScatterPlan -> ([TaskDef], [EdgeDef], Text)
buildTree plan
  | null plan.partitions =
      let key0 = reductionTaskKey plan.groupKey 0 1
          node = mkTask key0 plan.aggregateExecutor [] ["result"] plan.aggregateProfile
      in ([node], [], key0)
  | otherwise = go 0 (map (partitionTaskKey plan.groupKey) [1..length plan.partitions]) [] []
 where
  go depth previous tasksAcc edgesAcc =
    let groups = chunksOf plan.fanIn previous
        final = length groups == 1
        nodes = zipWith (makeNode depth final) [1..] groups
        edgesNow = concat (zipWith (makeEdges depth) [1..] groups)
        generated = map fst nodes
        keys = map snd nodes
    in if final
       then (tasksAcc <> generated, edgesAcc <> edgesNow, head keys)
       else go (depth+1) keys (tasksAcc <> generated) (edgesAcc <> edgesNow)
  makeNode depth final ordinal children =
    let key0 = reductionTaskKey plan.groupKey depth ordinal
        inputPorts = map ("i" <>) (map pad6 [1..length children])
    in (mkTask key0 plan.aggregateExecutor inputPorts
          [if final then "result" else "value"] plan.aggregateProfile,key0)
  makeEdges depth ordinal children =
    let target = reductionTaskKey plan.groupKey depth ordinal
    in zipWith (\index child -> EdgeDef child "value" target ("i" <> pad6 index))
         [1..] children

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size values = let (firstChunk,rest) = splitAt size values
                       in firstChunk : chunksOf size rest
