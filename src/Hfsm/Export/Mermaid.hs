module Hfsm.Export.Mermaid ( exportMermaid ) where

import Data.Text (Text, pack)

import Hfsm.Graph.Def (MachineGraph)

exportMermaid :: MachineGraph -> Text
exportMermaid graph =
  -- TODO: provide a proper Mermaid export.
  pack $ show graph