module Hfsm.Validate.History
  ( checkHistory
  ) where

import Hfsm.Graph.Def (MachineGraph)
import Hfsm.Validate.Error (ValidateErr)

-- | v0.1 placeholder pass.
--
-- The current compiled graph does not expose explicit history or re-entry nodes,
-- so there is nothing history-specific to validate yet. We keep the pass in
-- place so the validation pipeline stays stable and future history semantics can
-- be added without reshaping the module graph.
checkHistory :: MachineGraph -> [ValidateErr]
checkHistory _ = []