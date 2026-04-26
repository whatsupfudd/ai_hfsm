{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Graph.Def
  ( MachineGraph(..)
  , RegionNd(..)
  , StateNd(..)
  , CaseNd(..)
  , RouteEd(..)
  , RoutePlan(..)
  , RouteTarget(..)
  , WaitPlanG(..)
  , SpawnPlanG(..)
  , JoinPlanG(..)
  , JoinMode(..)
  , JoinNd(..)
  , TimerPlanG(..)
  , TimerNd(..)
  , BreakPlanG(..)
  ) where

import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import Hfsm.Core.Digest (GraphDigest)
import Hfsm.Core.Meta (MetaSpec)
import Hfsm.Core.Name (CaseName, JoinName, MachineName, StateName, TimerName)
import Hfsm.Core.Path (RegionPath, StatePath)
import Hfsm.Core.Ref (CaseRef, JoinRef, RegionRef, RouteRef, StateRef, TimerRef)
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Spec.Def (StateKind)

data MachineGraph = MachineGraph
  { machine :: MachineName
  , version :: MachineVersion
  , digest :: GraphDigest
  , root :: RegionRef
  , regions :: IntMap RegionNd
  , states :: IntMap StateNd
  , cases :: IntMap CaseNd
  , routes :: IntMap RouteEd
  , joins :: IntMap JoinNd
  , timers :: IntMap TimerNd
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data RegionNd = RegionNd
  { ref :: RegionRef
  , parent :: Maybe StateRef
  , initial :: StateRef
  , states :: Vector StateRef
  , path :: RegionPath
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data StateNd = StateNd
  { ref :: StateRef
  , parent :: RegionRef
  , name :: StateName
  , kind :: StateKind
  , entry :: Maybe CaseRef
  , cases :: Vector CaseRef
  , routes :: Vector RouteRef
  , child :: Maybe RegionRef
  , path :: StatePath
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data CaseNd = CaseNd
  { ref :: CaseRef
  , state :: StateRef
  , name :: Maybe CaseName
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data RouteEd = RouteEd
  { ref :: RouteRef
  , state :: StateRef
  , caseRef :: Maybe CaseRef
  , handoff :: Text
  , plan :: RoutePlan
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data RoutePlan = RoutePlan
  { target :: RouteTarget
  , wait :: WaitPlanG
  , spawn :: [SpawnPlanG]
  , join :: JoinPlanG
  , timers :: [TimerPlanG]
  , break :: [BreakPlanG]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data RouteTarget
  = StayRt
  | GotoRt StateRef
  | CompleteRt
  | FailRt Text
  deriving stock (Eq, Ord, Show, Read, Generic)

data WaitPlanG
  = WaitNoneWg
  | WaitSignalWg
  | WaitJoinWg JoinRef
  | WaitTimerWg TimerRef
  deriving stock (Eq, Ord, Show, Read, Generic)

data SpawnPlanG = SpawnPlanG
  { child :: Text
  , input :: Value
  , key :: Maybe Text
  , join :: Maybe JoinRef
  }
  deriving stock (Eq, Show, Read, Generic)

instance Ord SpawnPlanG where
  compare a b =
    compare a.child b.child
      <> compareValue a.input b.input
      <> compare a.key b.key
      <> compare a.join b.join

data JoinPlanG
  = JoinNoneJg
  | JoinAllJg JoinRef
  | JoinAnyJg JoinRef
  | JoinCountJg JoinRef Int
  deriving stock (Eq, Ord, Show, Read, Generic)

data JoinMode
  = AllJm
  | AnyJm
  | CountJm Int
  deriving stock (Eq, Ord, Show, Read, Generic)

data JoinNd = JoinNd
  { ref :: JoinRef
  , name :: Maybe JoinName
  , mode :: JoinMode
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data TimerPlanG = TimerPlanG
  { ref :: TimerRef
  , delay :: NominalDiffTime
  , payload :: Maybe Value
  }
  deriving stock (Eq, Show, Read, Generic)

instance Ord TimerPlanG where
  compare a b =
    compare a.ref b.ref
      <> compare a.delay b.delay
      <> compareMaybeValue a.payload b.payload

data TimerNd = TimerNd
  { ref :: TimerRef
  , name :: Maybe TimerName
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data BreakPlanG
  = BreakBeforeCaseBg
  | BreakAfterCaseBg
  | BreakBeforeRouteBg
  | BreakAfterRouteBg
  | BreakBeforeCommitBg
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

compareMaybeValue :: Maybe Value -> Maybe Value -> Ordering
compareMaybeValue Nothing Nothing = EQ
compareMaybeValue Nothing (Just _) = LT
compareMaybeValue (Just _) Nothing = GT
compareMaybeValue (Just a) (Just b) = compareValue a b

compareValue :: Value -> Value -> Ordering
compareValue a b =
  compare (valueTag a) (valueTag b) <>
    case (a, b) of
      (Null, Null) -> EQ
      (Bool x, Bool y) -> compare x y
      (Number x, Number y) -> compare x y
      (String x, String y) -> compare x y
      (Array xs, Array ys) -> compareValueList (V.toList xs) (V.toList ys)
      (Object xs, Object ys) -> compareObjectPairs (normalizeObject xs) (normalizeObject ys)
      _ -> EQ

valueTag :: Value -> Int
valueTag val =
  case val of
    Null -> 0
    Bool _ -> 1
    Number _ -> 2
    String _ -> 3
    Array _ -> 4
    Object _ -> 5

normalizeObject :: KM.KeyMap Value -> [(Text, Value)]
normalizeObject =
  sortOn fst . fmap (\(k, v) -> (K.toText k, v)) . KM.toList

compareObjectPairs :: [(Text, Value)] -> [(Text, Value)] -> Ordering
compareObjectPairs [] [] = EQ
compareObjectPairs [] (_ : _) = LT
compareObjectPairs (_ : _) [] = GT
compareObjectPairs ((ka, va) : as) ((kb, vb) : bs) =
  compare ka kb <> compareValue va vb <> compareObjectPairs as bs

compareValueList :: [Value] -> [Value] -> Ordering
compareValueList [] [] = EQ
compareValueList [] (_ : _) = LT
compareValueList (_ : _) [] = GT
compareValueList (a : as) (b : bs) =
  compareValue a b <> compareValueList as bs