{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | DF-01 finite, static, bounded DAG. No dynamically generated tasks,
-- repeated edges, scatter/gather, cycles, or mutable task definitions.
module Hfsm.Flow.Spec
  ( FlowSpec(..), TaskDef(..), EdgeDef(..), FlowSpecErr(..)
  , validateFlowSpec, topologicalOrder, incomingEdges, outgoingEdges
  ) where

import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data FlowSpec = FlowSpec
  { tasks :: [TaskDef]
  , edges :: [EdgeDef]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TaskDef = TaskDef
  { key :: Text
  , executor :: Text
  , inputs :: [Text]
  , outputs :: [Text]
  , cpu :: Int              -- ^ Integer compute units, minimum 1
  , memoryMb :: Int         -- ^ Reserved memory units, minimum 1
  , capabilities :: [Text]  -- ^ Additional CT-06 worker capabilities
  , maxAttempts :: Int      -- ^ Includes the initial execution
  , timeoutSeconds :: Int
  , priority :: Int         -- ^ -20 .. 20
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data EdgeDef = EdgeDef
  { fromTask :: Text
  , fromPort :: Text
  , toTask :: Text
  , toPort :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FlowSpecErr = FlowSpecErr Text
  deriving stock (Eq, Show)

failWith :: Text -> Either FlowSpecErr a
failWith = Left . FlowSpecErr

unique :: Ord a => [a] -> Bool
unique xs = length xs == S.size (S.fromList xs)

validKey :: Text -> Bool
validKey x = not (T.null x) && T.length x <= 128 && T.all allowed x
 where
  allowed c = c == '_' || c == '-' || c == '.' || c == ':' ||
              (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
              (c >= '0' && c <= '9')

-- | A graph is valid when every required port has exactly one incoming edge,
-- every referenced output is declared, and Kahn's traversal visits every task.
-- Root tasks have no input ports and may start immediately.
validateFlowSpec :: FlowSpec -> Either FlowSpecErr ()
validateFlowSpec spec = do
  let ts = spec.tasks
      es = spec.edges
      keys = map (.key) ts
      byKey = M.fromList [(t.key,t) | t <- ts]
  unless (not (null ts) && length ts <= 256) $
    failWith "flow must contain between 1 and 256 tasks"
  unless (length es <= 4096) $ failWith "flow exceeds 4096 edges"
  unless (unique keys) $ failWith "duplicate task key"
  mapM_ validateTask ts
  unless (unique es) $ failWith "duplicate edge"
  unless (unique [(e.toTask,e.toPort) | e <- es]) $
    failWith "multiple producers for one input port"
  mapM_ (validateEdge byKey) es
  let expected = sort [(t.key,p) | t <- ts, p <- t.inputs]
      provided = sort [(e.toTask,e.toPort) | e <- es]
  unless (expected == provided) $ failWith "every declared input port needs one edge"
  _ <- topologicalOrder spec
  pure ()
 where
  validateTask :: TaskDef -> Either FlowSpecErr ()
  validateTask t = do
    unless (validKey t.key && validKey t.executor) $ failWith "invalid task/executor key"
    unless (length t.inputs <= 64 && length t.outputs <= 64 &&
            unique t.inputs && unique t.outputs &&
            all validKey t.inputs && all validKey t.outputs) $
      failWith ("invalid or repeated ports in task " <> t.key)
    unless (t.cpu >= 1 && t.cpu <= 1024 &&
            t.memoryMb >= 1 && t.memoryMb <= 1048576 &&
            t.maxAttempts >= 1 && t.maxAttempts <= 100 &&
            t.timeoutSeconds >= 1 && t.timeoutSeconds <= 86400 &&
            t.priority >= (-20) && t.priority <= 20 &&
            length t.capabilities <= 32 && all validKey t.capabilities &&
            unique t.capabilities) $
      failWith ("invalid resource, retry or capability bounds for " <> t.key)
  validateEdge :: M.Map Text TaskDef -> EdgeDef -> Either FlowSpecErr ()
  validateEdge lookupTask e = do
    unless (e.fromTask /= e.toTask) $ failWith "self cycle"
    src <- maybe (failWith "unknown edge source") Right (M.lookup e.fromTask lookupTask)
    dst <- maybe (failWith "unknown edge destination") Right (M.lookup e.toTask lookupTask)
    unless (e.fromPort `elem` src.outputs && e.toPort `elem` dst.inputs) $
      failWith "edge references an undeclared output/input port"

-- | Deterministic Kahn traversal. Can also be used when diagnosing cycles,
-- independently of full port validation (does not recurse into validation).
topologicalOrder :: FlowSpec -> Either FlowSpecErr [Text]
topologicalOrder spec = loop indegrees roots []
 where
  keys = map (.key) spec.tasks
  indegrees = M.fromList
    [(k, length [e | e <- spec.edges, e.toTask == k]) | k <- keys]
  roots = sort [k | (k,n) <- M.toList indegrees, n == 0]
  loop counts [] visited
    | length visited == length keys = Right (reverse visited)
    | otherwise = failWith "graph contains a directed cycle"
  loop counts (k:ks) visited =
    let outs = [e.toTask | e <- spec.edges, e.fromTask == k]
        (next, newlyReady) = foldl decrement (counts,[]) outs
        decrement (acc, ready) dst =
          let remaining = M.findWithDefault 0 dst acc - 1
          in (M.insert dst remaining acc, if remaining == 0 then dst:ready else ready)
    in loop next (sort (ks ++ newlyReady)) (k:visited)

incomingEdges :: Text -> FlowSpec -> [EdgeDef]
incomingEdges k spec = filter (\edge -> edge.toTask == k) spec.edges

outgoingEdges :: Text -> FlowSpec -> [EdgeDef]
outgoingEdges k spec = filter (\edge -> edge.fromTask == k) spec.edges
