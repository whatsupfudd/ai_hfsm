{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Migrate.Plan
  ( MachineKey(..)
  , MigrateErr(..)
  , MigratePlan(..)
  , mkMachineKey
  , renderMachineKey
  , sameMachineName
  , sameMachineVersion
  , sameMachineDigest
  , sameMachineKey
  , identityCtx
  , identityState
  , identityWait
  , ctxErr
  , stateErr
  , waitErr
  , incompatibleErr
  , mkMigratePlan
  , setCtx
  , setState
  , setWait
  , clearWait
  , applyCtx
  , applyState
  , applyWait
  , planHasDiffs
  , planHasWait
  , incompatibleDiffs
  , planHasIncompatibleDiffs
  , compatiblePlan
  , renderMigrateErr
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Hfsm.Core.Digest (SpecDigest, specDigestText)
import Hfsm.Core.Name (MachineName, machineNameText)
import Hfsm.Core.Ref (StateRef, stateRefWord32)
import Hfsm.Core.Version (MachineVersion, machineVersionText)
import Hfsm.Migrate.Diff (GraphDiff(..))

data MachineKey = MachineKey
  { name :: MachineName
  , version :: MachineVersion
  , digest :: SpecDigest
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data MigrateErr
  = CtxEr Text
  | StateEr StateRef Text
  | WaitEr Text
  | IncompatibleEr Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data MigratePlan = MigratePlan
  { from :: MachineKey
  , to :: MachineKey
  , diffs :: [GraphDiff]
  , ctx :: Value -> Either MigrateErr Value
  , state :: StateRef -> Value -> Either MigrateErr StateRef
  , wait :: Maybe (Value -> Either MigrateErr Value)
  }

instance Show MigratePlan where
  showsPrec d plan =
    showParen (d > 10) $
      showString "MigratePlan {from = "
      . shows plan.from
      . showString ", to = "
      . shows plan.to
      . showString ", diffs = "
      . shows plan.diffs
      . showString ", ctx = <fn>, state = <fn>, wait = "
      . showString (if isJust plan.wait then "Just <fn>" else "Nothing")
      . showString "}"

mkMachineKey :: MachineName -> MachineVersion -> SpecDigest -> MachineKey
mkMachineKey = MachineKey

renderMachineKey :: MachineKey -> Text
renderMachineKey key =
  machineNameText key.name <> ":" <> machineVersionText key.version <> "#" <> specDigestText key.digest

sameMachineName :: MachineKey -> MachineKey -> Bool
sameMachineName a b = a.name == b.name

sameMachineVersion :: MachineKey -> MachineKey -> Bool
sameMachineVersion a b = a.version == b.version

sameMachineDigest :: MachineKey -> MachineKey -> Bool
sameMachineDigest a b = a.digest == b.digest

sameMachineKey :: MachineKey -> MachineKey -> Bool
sameMachineKey a b = sameMachineName a b && sameMachineVersion a b && sameMachineDigest a b

identityCtx :: Value -> Either MigrateErr Value
identityCtx = Right

identityState :: StateRef -> Value -> Either MigrateErr StateRef
identityState ref _ = Right ref

identityWait :: Value -> Either MigrateErr Value
identityWait = Right

ctxErr :: Text -> MigrateErr
ctxErr = CtxEr

stateErr :: StateRef -> Text -> MigrateErr
stateErr = StateEr

waitErr :: Text -> MigrateErr
waitErr = WaitEr

incompatibleErr :: Text -> MigrateErr
incompatibleErr = IncompatibleEr

mkMigratePlan :: MachineKey -> MachineKey -> [GraphDiff] -> MigratePlan
mkMigratePlan from0 to0 diffs0 =
  MigratePlan
    { from = from0
    , to = to0
    , diffs = diffs0
    , ctx = identityCtx
    , state = identityState
    , wait = Nothing
    }

setCtx :: (Value -> Either MigrateErr Value) -> MigratePlan -> MigratePlan
setCtx ctx0 plan = plan {ctx = ctx0}

setState :: (StateRef -> Value -> Either MigrateErr StateRef) -> MigratePlan -> MigratePlan
setState state0 plan = plan {state = state0}

setWait :: (Value -> Either MigrateErr Value) -> MigratePlan -> MigratePlan
setWait wait0 plan = plan {wait = Just wait0}

clearWait :: MigratePlan -> MigratePlan
clearWait plan = plan {wait = Nothing}

applyCtx :: Value -> MigratePlan -> Either MigrateErr Value
applyCtx value plan = plan.ctx value

applyState :: StateRef -> Value -> MigratePlan -> Either MigrateErr StateRef
applyState ref value plan = plan.state ref value

applyWait :: Maybe Value -> MigratePlan -> Either MigrateErr (Maybe Value)
applyWait mValue plan =
  case mValue of
    Nothing -> Right Nothing
    Just value ->
      case plan.wait of
        Nothing -> Right (Just value)
        Just migrateWait -> Just <$> migrateWait value

planHasDiffs :: MigratePlan -> Bool
planHasDiffs plan = not (null plan.diffs)

planHasWait :: MigratePlan -> Bool
planHasWait plan = isJust plan.wait

incompatibleDiffs :: MigratePlan -> [GraphDiff]
incompatibleDiffs plan = filter isIncompatibleDiff plan.diffs

planHasIncompatibleDiffs :: MigratePlan -> Bool
planHasIncompatibleDiffs plan = not (null (incompatibleDiffs plan))

compatiblePlan :: MigratePlan -> Bool
compatiblePlan plan = sameMachineName plan.from plan.to && not (planHasIncompatibleDiffs plan)

renderMigrateErr :: MigrateErr -> Text
renderMigrateErr err =
  case err of
    CtxEr msg ->
      "migration context error: " <> msg
    StateEr ref msg ->
      "migration state error at state " <> renderStateRef ref <> ": " <> msg
    WaitEr msg ->
      "migration wait error: " <> msg
    IncompatibleEr msg ->
      "migration incompatible error: " <> msg

isIncompatibleDiff :: GraphDiff -> Bool
isIncompatibleDiff diff =
  case diff of
    IncompatibleGd _ -> True
    _ -> False

renderStateRef :: StateRef -> Text
renderStateRef ref = T.pack (show (stateRefWord32 ref))