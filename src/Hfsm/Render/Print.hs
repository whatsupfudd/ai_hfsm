module Hfsm.Render.Print where

import Data.Text (Text)
import qualified Data.Text as T

import Hfsm.Compile (CompiledMachine (..))
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Core.Digest (graphDigestText)
import Hfsm.Spec.Def (MachineSpec (..))
import Hfsm.Graph.Def (MachineGraph (..))


ppMachine :: CompiledMachine st sg hf ctx cmd child -> Text
ppMachine machine =
  T.intercalate "\n" $
    [ "Machine: " <> machineNameText machine.spec.name
    , "Version: " <> machineVersionText machine.spec.version
    , "Digest: " <> graphDigestText machine.graph.digest
    ]