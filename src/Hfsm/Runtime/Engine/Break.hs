module Hfsm.Runtime.Engine.Break
  ( matchBreaks
  , pauseInstance
  , resumeInstance
  ) where

import Data.Maybe (isJust)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.UUID (UUID)

import Hfsm.Core.Ref (StateRef)
import Hfsm.Runtime.Engine.Step (StepInput(..), StepPlan(..))
import Hfsm.Runtime.Model.Instance (InstanceRw(..))

import Hfsm.Runtime.Model
  ( BreakKind(..)
  , BreakRw(..)
  , BreakStatus(..)
  , StepKind(..)
  , StepRw(..)
  )
import Hfsm.Store.Class (BreakAppend(..), Store(..))


matchBreaks :: [BreakRw] -> StepInput -> StepPlan -> [BreakRw]
matchBreaks breakRws stepInput stepPlan =
  let hitAt = stepPlan.step.createdAt
  in fmap (markBreakHit hitAt) (filter (matchesBreak stepInput stepPlan) breakRws)

pauseInstance :: Monad m => Store m -> UUID -> BreakKind -> m ()
pauseInstance store instId breakKind =
  store.appendBreak [mkPauseAppend instId breakKind]

resumeInstance :: Monad m => Store m -> UUID -> m ()
resumeInstance store instId =
  store.appendBreak (fmap (mkResumeAppend instId) allBreakKinds)

matchesBreak :: StepInput -> StepPlan -> BreakRw -> Bool
matchesBreak stepInput stepPlan breakRw =
  breakRw.status == ArmedBs &&
  breakTargetsInst stepInput.instance_.uuid breakRw &&
  breakKindReached stepPlan breakRw.kind &&
  breakTargetsState stepPlan breakRw

breakTargetsInst :: UUID -> BreakRw -> Bool
breakTargetsInst instId breakRw =
  case breakRw.inst of
    Nothing -> True
    Just targetInst -> targetInst == instId

breakKindReached :: StepPlan -> BreakKind -> Bool
breakKindReached stepPlan breakKind =
  case breakKind of
    BeforeCaseBk -> hasCasePhase stepPlan
    AfterCaseBk -> hasCasePhase stepPlan
    BeforeRouteBk -> True
    AfterRouteBk -> True
    BeforeCommitBk -> True
    OnErrorBk -> False

hasCasePhase :: StepPlan -> Bool
hasCasePhase stepPlan =
  case stepPlan.step.kind of
    EntrySk -> True
    _ -> isJust stepPlan.step.caseRef

breakTargetsState :: StepPlan -> BreakRw -> Bool
breakTargetsState stepPlan breakRw =
  case breakRw.state of
    Nothing -> True
    Just stateRef -> stateRef == phaseStateRef breakRw.kind stepPlan

phaseStateRef :: BreakKind -> StepPlan -> StateRef
phaseStateRef breakKind stepPlan =
  case breakKind of
    BeforeCaseBk -> stepPlan.step.from
    AfterCaseBk -> stepPlan.step.from
    BeforeRouteBk -> stepPlan.step.from
    AfterRouteBk -> stepPlan.step.to
    BeforeCommitBk -> stepPlan.step.to
    OnErrorBk -> stepPlan.step.from

markBreakHit :: UTCTime -> BreakRw -> BreakRw
markBreakHit hitAt breakRw =
  breakRw
    { status = HitBs
    , updatedAt = hitAt
    }

mkPauseAppend :: UUID -> BreakKind -> BreakAppend
mkPauseAppend instId breakKind =
  BreakAppend
    { inst = Just instId
    , kind = breakKind
    , state = Nothing
    , status = ArmedBs
    , note = Just "pause requested"
    , now = adminBreakTime
    }

mkResumeAppend :: UUID -> BreakKind -> BreakAppend
mkResumeAppend instId breakKind =
  BreakAppend
    { inst = Just instId
    , kind = breakKind
    , state = Nothing
    , status = ClearedBs
    , note = Just "resume requested"
    , now = adminBreakTime
    }

allBreakKinds :: [BreakKind]
allBreakKinds = [minBound .. maxBound]

adminBreakTime :: UTCTime
adminBreakTime =
  UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)