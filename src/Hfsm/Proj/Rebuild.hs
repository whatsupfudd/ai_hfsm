module Hfsm.Proj.Rebuild
  ( rebuildActive
  , rebuildTrace
  , rebuildTree
  ) where

import Control.Applicative ((<|>))

import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down(..))
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

import Data.Aeson (Value)

import Hfsm.Proj.Active (ActiveProj (..), activeFromRows, touchActive)
import Hfsm.Proj.Queue (renderWaitKind, waitKindFromValue)
import Hfsm.Proj.Trace (TraceProj (..), sortTraceProjAsc, traceProjFromSteps)
import Hfsm.Proj.Tree (TreeProj (..), attachBlockedDesc, mkTreeProj, sortTreeProj)
import Hfsm.Runtime.Model (
    ChildRw (..)
  , InstanceRw (..)
  , InstanceStatus(..)
  , SnapshotRw (..)
  , StepRw (..)
  )


rebuildActive :: [InstanceRw] -> [SnapshotRw] -> [StepRw] -> [ActiveProj]
rebuildActive insts snaps steps =
  sortActiveProjs $
    fmap (rebuildActiveOne snapByUuid latestSnapByInst latestStepByInst) stableInsts
  where
    stableInsts = M.elems (buildInstanceByUuid insts)
    snapByUuid = buildSnapshotByUuid snaps
    latestSnapByInst = buildLatestSnapshotByInst snaps
    latestStepByInst = buildLatestStepByInst steps

rebuildTrace :: [StepRw] -> [TraceProj]
rebuildTrace = sortTraceProjAsc . traceProjFromSteps

rebuildTree :: [InstanceRw] -> [ChildRw] -> [TreeProj]
rebuildTree insts links =
  sortTreeProj $
    attachBlockedDesc $
      fmap (rebuildTreeOne instByUuid inboundByChild) stableInsts
  where
    instByUuid = buildInstanceByUuid insts
    inboundByChild = buildInboundChildByChild links
    stableInsts = M.elems instByUuid

rebuildActiveOne :: Map UUID SnapshotRw -> Map UUID SnapshotRw -> Map UUID StepRw -> InstanceRw -> ActiveProj
rebuildActiveOne snapByUuid latestSnapByInst latestStepByInst inst =
  let snap = resolveSnapshot snapByUuid latestSnapByInst inst
      blockedOn = blockedOnText inst snap
      active0 = activeFromRows inst snap blockedOn
      updatedAt' = maxTime active0.updatedAt (snapshotTimes snap <> stepTimes (M.lookup inst.uuid latestStepByInst))
  in if updatedAt' > active0.updatedAt then touchActive updatedAt' active0 else active0

rebuildTreeOne :: Map UUID InstanceRw -> Map UUID ChildRw -> InstanceRw -> TreeProj
rebuildTreeOne instByUuid inboundByChild inst =
  let inbound = M.lookup inst.uuid inboundByChild
      (rootUuid, depthIx) = resolveRootDepth instByUuid inboundByChild inst.uuid
      tree0 = mkTreeProj rootUuid depthIx inst inbound
  in case (inbound, inst.parent) of
       (Nothing, Just parentUuid) -> tree0 { parent = Just parentUuid }
       _ -> tree0

resolveSnapshot :: Map UUID SnapshotRw -> Map UUID SnapshotRw -> InstanceRw -> Maybe SnapshotRw
resolveSnapshot snapByUuid latestSnapByInst inst =
  case M.lookup inst.snap snapByUuid of
    Just snap | snap.inst == inst.uuid -> Just snap
    _ -> M.lookup inst.uuid latestSnapByInst

blockedOnText :: InstanceRw -> Maybe SnapshotRw -> Maybe Text
blockedOnText inst snap =
  case inst.status of
    WaitingIs -> Just (renderBlockedWait (snap >>= \x -> x.wait))
    PausedIs -> Just "break"
    _ -> Nothing

renderBlockedWait :: Maybe Value -> Text
renderBlockedWait Nothing = "wait"
renderBlockedWait (Just waitValue) = "wait:" <> renderWaitKind (waitKindFromValue waitValue)

resolveRootDepth :: Map UUID InstanceRw -> Map UUID ChildRw -> UUID -> (UUID, Int)
resolveRootDepth instByUuid inboundByChild startUuid =
  case go S.empty startUuid of
    Just rez -> rez
    Nothing -> (startUuid, 0)
  where
    go seen curUuid
      | S.member curUuid seen = Nothing
      | otherwise =
          case directParentUuid instByUuid inboundByChild curUuid of
            Just parentUuid | M.member parentUuid instByUuid ->
              fmap (\(rootUuid, depthIx) -> (rootUuid, depthIx + 1)) (go (S.insert curUuid seen) parentUuid)
            _ ->
              Just (curUuid, 0)

directParentUuid :: Map UUID InstanceRw -> Map UUID ChildRw -> UUID -> Maybe UUID
directParentUuid instByUuid inboundByChild instUuid =
  case M.lookup instUuid inboundByChild of
    Just link -> Just link.parent
    Nothing -> do
      inst <- M.lookup instUuid instByUuid
      inst.parent

sortActiveProjs :: [ActiveProj] -> [ActiveProj]
sortActiveProjs =
  sortOn (\proj -> (Down proj.updatedAt, proj.machine, proj.instance_))

buildInstanceByUuid :: [InstanceRw] -> Map UUID InstanceRw
buildInstanceByUuid =
  foldl' (\acc inst -> M.insertWith chooseNewerInstance inst.uuid inst acc) M.empty

buildSnapshotByUuid :: [SnapshotRw] -> Map UUID SnapshotRw
buildSnapshotByUuid =
  foldl' (\acc snap -> M.insertWith chooseNewerSnapshot snap.uuid snap acc) M.empty

buildLatestSnapshotByInst :: [SnapshotRw] -> Map UUID SnapshotRw
buildLatestSnapshotByInst =
  foldl' (\acc snap -> M.insertWith chooseNewerSnapshot snap.inst snap acc) M.empty

buildLatestStepByInst :: [StepRw] -> Map UUID StepRw
buildLatestStepByInst =
  foldl' (\acc step -> M.insertWith chooseNewerStep step.inst step acc) M.empty

buildInboundChildByChild :: [ChildRw] -> Map UUID ChildRw
buildInboundChildByChild =
  foldl' (\acc link -> M.insertWith chooseNewerChildLink link.child link acc) M.empty

chooseNewerInstance :: InstanceRw -> InstanceRw -> InstanceRw
chooseNewerInstance new old
  | instanceOrdKey new >= instanceOrdKey old = new
  | otherwise = old

chooseNewerSnapshot :: SnapshotRw -> SnapshotRw -> SnapshotRw
chooseNewerSnapshot new old
  | snapshotOrdKey new >= snapshotOrdKey old = new
  | otherwise = old

chooseNewerStep :: StepRw -> StepRw -> StepRw
chooseNewerStep new old
  | stepOrdKey new >= stepOrdKey old = new
  | otherwise = old

chooseNewerChildLink :: ChildRw -> ChildRw -> ChildRw
chooseNewerChildLink new old
  | childLinkOrdKey new >= childLinkOrdKey old = new
  | otherwise = old

instanceOrdKey :: InstanceRw -> (UTCTime, UTCTime, UUID)
instanceOrdKey inst = (inst.updatedAt, inst.createdAt, inst.uuid)

snapshotOrdKey :: SnapshotRw -> (UTCTime, UUID)
snapshotOrdKey snap = (snap.createdAt, snap.uuid)

stepOrdKey :: StepRw -> (UTCTime, UUID)
stepOrdKey step = (step.createdAt, step.uuid)

childLinkOrdKey :: ChildRw -> (UTCTime, UTCTime, UUID)
childLinkOrdKey link = (link.updatedAt, link.createdAt, link.uuid)

snapshotTimes :: Maybe SnapshotRw -> [UTCTime]
snapshotTimes Nothing = []
snapshotTimes (Just snap) = [snap.createdAt]

stepTimes :: Maybe StepRw -> [UTCTime]
stepTimes Nothing = []
stepTimes (Just step) = [step.createdAt]

maxTime :: UTCTime -> [UTCTime] -> UTCTime
maxTime = foldl' max