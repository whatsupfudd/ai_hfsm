{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Proj.Tree
  ( JoinStatus(..)
  , TreeProj(..)
  , mkRootTreeProj
  , mkChildTreeProj
  , mkTreeProj
  , setTreeRoot
  , setTreeDepth
  , setBlockedDesc
  , clearBlockedDesc
  , joinStatusFromLink
  , parseJoinStatus
  , renderJoinStatus
  , blockedInstanceStatus
  , treeIsRoot
  , treeHasParent
  , treeHasJoin
  , treeJoinOpen
  , treeJoinClosed
  , treeIsBlocked
  , treeHasBlockedDesc
  , treeIsActive
  , treeIsTerminal
  , sortTreeProj
  , attachBlockedDesc
  ) where

import Control.Applicative ((<|>))

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), withText)

import Hfsm.Core.Name (MachineName)
import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (JoinRef, StateRef)
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Runtime.Model.Child (ChildRw(..), ChildStatus(..), isChildTerminal)
import Hfsm.Runtime.Model.Instance (InstanceRw(..), InstanceStatus(..), instanceIsActive, instanceIsTerminal)


data JoinStatus
  = NoJoinJs
  | OpenJoinJs
  | ClosedJoinJs
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data TreeProj = TreeProj
  { inst :: UUID
  , parent :: Maybe UUID
  , root :: UUID
  , depth :: Int
  , machine :: MachineName
  , version :: MachineVersion
  , status :: InstanceStatus
  , state :: StateRef
  , path :: StatePath
  , key :: Maybe Text
  , childStatus :: Maybe ChildStatus
  , join :: Maybe JoinRef
  , joinStatus :: JoinStatus
  , blocked :: Bool
  , blockedDesc :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkRootTreeProj :: InstanceRw -> TreeProj
mkRootTreeProj instRw =
  mkTreeProj instRw.uuid 0 instRw Nothing

mkChildTreeProj :: UUID -> Int -> InstanceRw -> ChildRw -> TreeProj
mkChildTreeProj rootUuid depth' instRw childRw =
  mkTreeProj rootUuid depth' instRw (Just childRw)

mkTreeProj :: UUID -> Int -> InstanceRw -> Maybe ChildRw -> TreeProj
mkTreeProj rootUuid depth' instRw childRw =
  let
    parentUuid = instRw.parent <|> inboundParent childRw
    childStatus' = fmap (.status) childRw
    joinRef = childRw >>= \x -> x.join
    joinStatus' = joinStatusFromLink joinRef childStatus'
  in
  TreeProj
       { inst = instRw.uuid
       , parent = parentUuid
       , root = rootUuid
       , depth = normalizeDepth depth'
       , machine = instRw.machine
       , version = instRw.version
       , status = instRw.status
       , state = instRw.state
       , path = instRw.path
       , key = childRw >>= \x -> x.key
       , childStatus = childStatus'
       , join = joinRef
       , joinStatus = joinStatus'
       , blocked = blockedInstanceStatus instRw.status
       , blockedDesc = 0
       , createdAt = instRw.createdAt
       , updatedAt = instRw.updatedAt
       }

setTreeRoot :: UUID -> TreeProj -> TreeProj
setTreeRoot rootUuid treePr =
  treePr { root = rootUuid }

setTreeDepth :: Int -> TreeProj -> TreeProj
setTreeDepth depth' treePr =
  treePr { depth = normalizeDepth depth' }

setBlockedDesc :: Int -> TreeProj -> TreeProj
setBlockedDesc blockedDesc' treePr =
  treePr { blockedDesc = max 0 blockedDesc' }

clearBlockedDesc :: TreeProj -> TreeProj
clearBlockedDesc = setBlockedDesc 0

joinStatusFromLink :: Maybe JoinRef -> Maybe ChildStatus -> JoinStatus
joinStatusFromLink joinRef childStatus =
  case joinRef of
    Nothing -> NoJoinJs
    Just _ ->
      case childStatus of
        Nothing -> OpenJoinJs
        Just st
          | isChildTerminal st -> ClosedJoinJs
          | otherwise -> OpenJoinJs

parseJoinStatus :: Text -> Maybe JoinStatus
parseJoinStatus raw =
  case T.toLower (T.strip raw) of
    "none" -> Just NoJoinJs
    "no-join" -> Just NoJoinJs
    "nojoin" -> Just NoJoinJs
    "open" -> Just OpenJoinJs
    "closed" -> Just ClosedJoinJs
    _ -> Nothing

renderJoinStatus :: JoinStatus -> Text
renderJoinStatus joinStatus =
  case joinStatus of
    NoJoinJs -> "none"
    OpenJoinJs -> "open"
    ClosedJoinJs -> "closed"

blockedInstanceStatus :: InstanceStatus -> Bool
blockedInstanceStatus status =
  case status of
    WaitingIs -> True
    PausedIs -> True
    RunningIs -> False
    DoneIs -> False
    FailedIs -> False
    CancelledIs -> False

treeIsRoot :: TreeProj -> Bool
treeIsRoot treePr =
  case treePr.parent of
    Nothing -> True
    Just _ -> False

treeHasParent :: TreeProj -> Bool
treeHasParent = isJust . (.parent)

treeHasJoin :: TreeProj -> Bool
treeHasJoin = isJust . (.join)

treeJoinOpen :: TreeProj -> Bool
treeJoinOpen treePr =
  treePr.joinStatus == OpenJoinJs

treeJoinClosed :: TreeProj -> Bool
treeJoinClosed treePr =
  treePr.joinStatus == ClosedJoinJs

treeIsBlocked :: TreeProj -> Bool
treeIsBlocked = (.blocked)

treeHasBlockedDesc :: TreeProj -> Bool
treeHasBlockedDesc treePr =
  treePr.blockedDesc > 0

treeIsActive :: TreeProj -> Bool
treeIsActive = instanceIsActive . (.status)

treeIsTerminal :: TreeProj -> Bool
treeIsTerminal = instanceIsTerminal . (.status)

sortTreeProj :: [TreeProj] -> [TreeProj]
sortTreeProj =
  sortOn (\treePr -> (treePr.root, treePr.depth, treePr.parent, treePr.createdAt, treePr.inst))

attachBlockedDesc :: [TreeProj] -> [TreeProj]
attachBlockedDesc treePrs =
  let childMap = buildChildMap treePrs
      blockedMap = M.fromList (fmap (\treePr -> (treePr.inst, blockedDescCount childMap S.empty treePr.inst)) treePrs)
  in fmap (\treePr -> treePr { blockedDesc = M.findWithDefault 0 treePr.inst blockedMap }) treePrs

instance ToJSON JoinStatus where
  toJSON = String . renderJoinStatus

instance FromJSON JoinStatus where
  parseJSON =
    withText "JoinStatus" $ \txt ->
      case parseJoinStatus txt of
        Just joinStatus -> pure joinStatus
        Nothing -> fail ("invalid join status: " <> T.unpack txt)

inboundParent :: Maybe ChildRw -> Maybe UUID
inboundParent childRw =
  childRw >>= \x -> Just x.parent

normalizeDepth :: Int -> Int
normalizeDepth = max 0

buildChildMap :: [TreeProj] -> Map UUID [TreeProj]
buildChildMap =
  foldr step M.empty
  where
    step :: TreeProj -> Map UUID [TreeProj] -> Map UUID [TreeProj]
    step treePr acc =
      case treePr.parent of
        Nothing -> acc
        Just parentUuid -> M.insertWith (<>) parentUuid [treePr] acc

blockedDescCount :: Map UUID [TreeProj] -> Set UUID -> UUID -> Int
blockedDescCount childMap seen instUuid
  | S.member instUuid seen = 0
  | otherwise =
      let seen' = S.insert instUuid seen
          children = M.findWithDefault [] instUuid childMap
      in sum (fmap (blockedNodeScore childMap seen') children)

blockedNodeScore :: Map UUID [TreeProj] -> Set UUID -> TreeProj -> Int
blockedNodeScore childMap seen treePr =
  let selfScore = if treePr.blocked then 1 else 0
      childScore = blockedDescCount childMap seen treePr.inst
  in selfScore + childScore