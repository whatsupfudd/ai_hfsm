module Hfsm.Spec.Build
  ( machine
  , region
  , regionNamed
  , state
  , atomicState
  , compositeState
  , terminalState
  , entry
  , on
  , onNamed
  , route
  , stay
  , goto
  , complete
  , failWith
  , noWait
  , waitSignal
  , waitJoin
  , waitTimer
  , spawnChild
  , spawnChildWithKey
  , noJoin
  , joinAll
  , joinAny
  , joinCount
  , scheduleTimer
  , breakBeforeRoute
  , breakAfterRoute
  , breakBeforeCommit
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Hfsm.Core.Codec (CodecSet)
import Hfsm.Core.Name (CaseName, JoinName, MachineName, RegionName, StateName, TimerName)
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Spec.Case (CaseFn, CaseSpec, EntryFn)
import qualified Hfsm.Spec.Case as Case
import Hfsm.Spec.Def (MachineSpec, RegionSpec, StateKind(..), StateSpec)
import qualified Hfsm.Spec.Def as Def
import Hfsm.Spec.Route (ControlPlan, RouteSpec, TimerPlan)
import qualified Hfsm.Spec.Route as Route

machine :: MachineName -> MachineVersion -> CodecSet ctx sg cmd -> RegionSpec st sg hf ctx cmd child -> MachineSpec st sg hf ctx cmd child
machine = Def.mkMachineSpec

region :: st -> [StateSpec st sg hf ctx cmd child] -> RegionSpec st sg hf ctx cmd child
region = Def.mkRegionSpec

regionNamed :: RegionName -> st -> [StateSpec st sg hf ctx cmd child] -> RegionSpec st sg hf ctx cmd child
regionNamed name initial states =
  Def.setRegionName (Just name) (Def.mkRegionSpec initial states)

state :: st -> StateName -> StateKind -> StateSpec st sg hf ctx cmd child
state = Def.mkStateSpec

atomicState :: st -> StateName -> StateSpec st sg hf ctx cmd child
atomicState key name = Def.mkStateSpec key name AtomicSk

compositeState :: st -> StateName -> StateSpec st sg hf ctx cmd child
compositeState key name = Def.mkStateSpec key name CompositeSk

terminalState :: st -> StateName -> StateSpec st sg hf ctx cmd child
terminalState key name = Def.mkStateSpec key name TerminalSk

entry :: EntryFn ctx hf cmd -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
entry = Def.setStateEntry

on :: (sg -> Bool) -> CaseFn sg ctx hf cmd -> CaseSpec sg ctx hf cmd
on = Case.mkCaseSpec

onNamed :: CaseName -> (sg -> Bool) -> CaseFn sg ctx hf cmd -> CaseSpec sg ctx hf cmd
onNamed = Case.namedCaseSpec

route :: hf -> ControlPlan st child -> RouteSpec st hf child
route = Route.mkRouteSpec

stay :: ControlPlan st child
stay = Route.stayPlan

goto :: st -> ControlPlan st child
goto = Route.gotoPlan

complete :: ControlPlan st child
complete = Route.completePlan

failWith :: Text -> ControlPlan st child
failWith = Route.failPlan

noWait :: ControlPlan st child -> ControlPlan st child
noWait = Route.noWait

waitSignal :: ControlPlan st child -> ControlPlan st child
waitSignal = Route.waitSignal

waitJoin :: JoinName -> ControlPlan st child -> ControlPlan st child
waitJoin = Route.waitJoin

waitTimer :: TimerName -> ControlPlan st child -> ControlPlan st child
waitTimer = Route.waitTimer

spawnChild :: child -> Value -> ControlPlan st child -> ControlPlan st child
spawnChild = Route.spawnChild

spawnChildWithKey :: child -> Value -> Text -> ControlPlan st child -> ControlPlan st child
spawnChildWithKey = Route.spawnChildWithKey

noJoin :: ControlPlan st child -> ControlPlan st child
noJoin = Route.noJoin

joinAll :: JoinName -> ControlPlan st child -> ControlPlan st child
joinAll = Route.joinAll

joinAny :: JoinName -> ControlPlan st child -> ControlPlan st child
joinAny = Route.joinAny

joinCount :: JoinName -> Int -> ControlPlan st child -> ControlPlan st child
joinCount = Route.joinCount

scheduleTimer :: TimerPlan -> ControlPlan st child -> ControlPlan st child
scheduleTimer = Route.scheduleTimer

breakBeforeRoute :: ControlPlan st child -> ControlPlan st child
breakBeforeRoute = Route.breakBeforeRoute

breakAfterRoute :: ControlPlan st child -> ControlPlan st child
breakAfterRoute = Route.breakAfterRoute

breakBeforeCommit :: ControlPlan st child -> ControlPlan st child
breakBeforeCommit = Route.breakBeforeCommit