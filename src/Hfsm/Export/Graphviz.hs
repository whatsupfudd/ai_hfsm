module Hfsm.Export.Graphviz ( exportDot ) where

import Data.Text (Text, pack)

import Hfsm.Graph.Def (MachineGraph(..))
import Hfsm.Core (machineNameText)
import Hfsm.Core.Name (regionNameText)

exportDot :: MachineGraph -> Text
exportDot graph =
  -- TODO: provide a proper DOT export.
  pack $ show graph
