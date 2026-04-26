{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Runtime.Registry
  ( MachineKey(..)
  , Registry(..)
  , RegistryErr(..)
  , emptyRegistry
  , machineKeyOf
  , register
  , registerMany
  , resolve
  , latest
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

import Hfsm.Compile (CompiledMachine(..))
import Hfsm.Core.Digest (SpecDigest)
import Hfsm.Core.Name (MachineName)
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Spec.Def (MachineSpec(..))

data MachineKey = MachineKey
  { name :: MachineName
  , version :: MachineVersion
  , digest :: SpecDigest
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data Registry st sg hf ctx cmd child = Registry
  { byKey :: Map MachineKey (CompiledMachine st sg hf ctx cmd child)
  , byName :: Map MachineName [MachineKey]
  }

data RegistryErr
  = DuplicateMachineEr MachineKey
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

emptyRegistry :: Registry st sg hf ctx cmd child
emptyRegistry =
  Registry
    { byKey = M.empty
    , byName = M.empty
    }

machineKeyOf :: CompiledMachine st sg hf ctx cmd child -> MachineKey
machineKeyOf compiled =
  MachineKey
    { name = compiled.spec.name
    , version = compiled.spec.version
    , digest = compiled.digest
    }

register ::
  CompiledMachine st sg hf ctx cmd child
  -> Registry st sg hf ctx cmd child
  -> Either RegistryErr (Registry st sg hf ctx cmd child)
register compiled registry =
  let key = machineKeyOf compiled
  in
  if M.member key registry.byKey
    then Left (DuplicateMachineEr key)
    else
      Right
        Registry
          { byKey = M.insert key compiled registry.byKey
          , byName = insertNameKey key registry.byName
          }

registerMany ::
  [CompiledMachine st sg hf ctx cmd child]
  -> Registry st sg hf ctx cmd child
  -> Either RegistryErr (Registry st sg hf ctx cmd child)
registerMany compileds registry0 = foldM (flip register) registry0 compileds

resolve ::
  MachineKey
  -> Registry st sg hf ctx cmd child
  -> Maybe (CompiledMachine st sg hf ctx cmd child)
resolve key registry =
  M.lookup key registry.byKey

-- latest is "most recently registered for this machine name".
-- The registry does not impose any semantic ordering on MachineVersion.
latest :: MachineName -> Registry st sg hf ctx cmd child -> Maybe MachineKey
latest machineName registry = do
  keys <- M.lookup machineName registry.byName
  find (\key -> M.member key registry.byKey) keys

insertNameKey :: MachineKey -> Map MachineName [MachineKey] -> Map MachineName [MachineKey]
insertNameKey key =
  M.alter step key.name
  where
    step Nothing = Just [key]
    step (Just keys) = Just (key : filter (/= key) keys)