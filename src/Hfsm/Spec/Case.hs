{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Spec.Case
  ( ReactEnv(..)
  , ReactRez(..)
  , ReactErr(..)
  , EntryFn
  , CaseFn
  , CaseSpec(..)
  , mkReactEnv
  , withAttempt
  , withCause
  , insertChildValue
  , deleteChildValue
  , hasChildValue
  , insertTimerKey
  , deleteTimerKey
  , hasTimerKey
  , mkReactRez
  , addCmd
  , addCmds
  , addNote
  , addNotes
  , mapNext
  , mapHandoff
  , mapCmd
  , decodeErr
  , rejectErr
  , domainErr
  , invariantErr
  , renderReactErr
  , mkCaseSpec
  , namedCaseSpec
  , setCaseName
  , clearCaseName
  , setCaseMeta
  , mergeCaseMeta
  , matchesCase
  , findCaseSpec
  , runEntryFn
  , runCaseFn
  , runCaseSpec
  ) where

import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

import Data.Aeson (Value)

import Hfsm.Core.Meta (MetaSpec, emptyMetaSpec, mergeMetaSpec)
import Hfsm.Core.Name (CaseName)

data ReactEnv = ReactEnv
  { now :: UTCTime
  , attempt :: Int
  , cause :: Maybe Value
  , child :: Map Text Value
  , timer :: Set Text
  }
  deriving stock (Eq, Show, Read)

data ReactRez ctx hf cmd = ReactRez
  { next :: ctx
  , handoff :: hf
  , cmds :: [cmd]
  , note :: [Text]
  }
  deriving stock (Eq, Show, Read)

data ReactErr
  = DecodeEr Text
  | RejectEr Text
  | DomainEr Text
  | InvariantEr Text
  deriving stock (Eq, Ord, Show, Read)

type EntryFn ctx hf cmd = ReactEnv -> ctx -> Either ReactErr (ReactRez ctx hf cmd)

type CaseFn sg ctx hf cmd = ReactEnv -> sg -> ctx -> Either ReactErr (ReactRez ctx hf cmd)

data CaseSpec sg ctx hf cmd = CaseSpec
  { name :: Maybe CaseName
  , match :: sg -> Bool
  , react :: CaseFn sg ctx hf cmd
  , meta :: MetaSpec
  }

instance Show (CaseSpec sg ctx hf cmd) where
  show spec =
    "CaseSpec {name = " <> show spec.name <>
    ", match = <function>, react = <function>, meta = " <> show spec.meta <> "}"

mkReactEnv :: UTCTime -> ReactEnv
mkReactEnv now' =
  ReactEnv
    { now = now'
    , attempt = 0
    , cause = Nothing
    , child = M.empty
    , timer = S.empty
    }

withAttempt :: Int -> ReactEnv -> ReactEnv
withAttempt attempt' env =
  env { attempt = max 0 attempt' }

withCause :: Maybe Value -> ReactEnv -> ReactEnv
withCause cause' env =
  env { cause = cause' }

insertChildValue :: Text -> Value -> ReactEnv -> ReactEnv
insertChildValue rawKey value env =
  case normalizeKey rawKey of
    Nothing -> env
    Just key' -> env { child = M.insert key' value env.child }

deleteChildValue :: Text -> ReactEnv -> ReactEnv
deleteChildValue rawKey env =
  case normalizeKey rawKey of
    Nothing -> env
    Just key' -> env { child = M.delete key' env.child }

hasChildValue :: Text -> ReactEnv -> Bool
hasChildValue rawKey env =
  case normalizeKey rawKey of
    Nothing -> False
    Just key' -> M.member key' env.child

insertTimerKey :: Text -> ReactEnv -> ReactEnv
insertTimerKey rawKey env =
  case normalizeKey rawKey of
    Nothing -> env
    Just key' -> env { timer = S.insert key' env.timer }

deleteTimerKey :: Text -> ReactEnv -> ReactEnv
deleteTimerKey rawKey env =
  case normalizeKey rawKey of
    Nothing -> env
    Just key' -> env { timer = S.delete key' env.timer }

hasTimerKey :: Text -> ReactEnv -> Bool
hasTimerKey rawKey env =
  case normalizeKey rawKey of
    Nothing -> False
    Just key' -> S.member key' env.timer

mkReactRez :: ctx -> hf -> ReactRez ctx hf cmd
mkReactRez next' handoff' =
  ReactRez
    { next = next'
    , handoff = handoff'
    , cmds = []
    , note = []
    }

addCmd :: cmd -> ReactRez ctx hf cmd -> ReactRez ctx hf cmd
addCmd cmd' rez =
  rez { cmds = rez.cmds <> [cmd'] }

addCmds :: [cmd] -> ReactRez ctx hf cmd -> ReactRez ctx hf cmd
addCmds cmds' rez =
  rez { cmds = rez.cmds <> cmds' }

addNote :: Text -> ReactRez ctx hf cmd -> ReactRez ctx hf cmd
addNote rawNote rez =
  case normalizeKey rawNote of
    Nothing -> rez
    Just note' -> rez { note = rez.note <> [note'] }

addNotes :: [Text] -> ReactRez ctx hf cmd -> ReactRez ctx hf cmd
addNotes notes' rez =
  foldl' (\acc msg -> addNote msg acc) rez notes'

mapNext :: (ctx1 -> ctx2) -> ReactRez ctx1 hf cmd -> ReactRez ctx2 hf cmd
mapNext f rez =
  ReactRez
    { next = f rez.next
    , handoff = rez.handoff
    , cmds = rez.cmds
    , note = rez.note
    }

mapHandoff :: (hf1 -> hf2) -> ReactRez ctx hf1 cmd -> ReactRez ctx hf2 cmd
mapHandoff f rez =
  ReactRez
    { next = rez.next
    , handoff = f rez.handoff
    , cmds = rez.cmds
    , note = rez.note
    }

mapCmd :: (cmd1 -> cmd2) -> ReactRez ctx hf cmd1 -> ReactRez ctx hf cmd2
mapCmd f rez =
  ReactRez
    { next = rez.next
    , handoff = rez.handoff
    , cmds = fmap f rez.cmds
    , note = rez.note
    }

decodeErr :: Text -> ReactErr
decodeErr = DecodeEr

rejectErr :: Text -> ReactErr
rejectErr = RejectEr

domainErr :: Text -> ReactErr
domainErr = DomainEr

invariantErr :: Text -> ReactErr
invariantErr = InvariantEr

renderReactErr :: ReactErr -> Text
renderReactErr err =
  case err of
    DecodeEr msg -> "reaction decode error: " <> msg
    RejectEr msg -> "reaction rejected: " <> msg
    DomainEr msg -> "reaction domain error: " <> msg
    InvariantEr msg -> "reaction invariant error: " <> msg

mkCaseSpec :: (sg -> Bool) -> CaseFn sg ctx hf cmd -> CaseSpec sg ctx hf cmd
mkCaseSpec match' react' =
  CaseSpec
    { name = Nothing
    , match = match'
    , react = react'
    , meta = emptyMetaSpec
    }

namedCaseSpec :: CaseName -> (sg -> Bool) -> CaseFn sg ctx hf cmd -> CaseSpec sg ctx hf cmd
namedCaseSpec name' match' react' =
  CaseSpec
    { name = Just name'
    , match = match'
    , react = react'
    , meta = emptyMetaSpec
    }

setCaseName :: CaseName -> CaseSpec sg ctx hf cmd -> CaseSpec sg ctx hf cmd
setCaseName name' spec =
  spec { name = Just name' }

clearCaseName :: CaseSpec sg ctx hf cmd -> CaseSpec sg ctx hf cmd
clearCaseName spec =
  spec { name = Nothing }

setCaseMeta :: MetaSpec -> CaseSpec sg ctx hf cmd -> CaseSpec sg ctx hf cmd
setCaseMeta meta' spec =
  spec { meta = meta' }

mergeCaseMeta :: MetaSpec -> CaseSpec sg ctx hf cmd -> CaseSpec sg ctx hf cmd
mergeCaseMeta meta' spec =
  spec { meta = mergeMetaSpec spec.meta meta' }

matchesCase :: sg -> CaseSpec sg ctx hf cmd -> Bool
matchesCase sg spec =
  spec.match sg

findCaseSpec :: sg -> [CaseSpec sg ctx hf cmd] -> Maybe (CaseSpec sg ctx hf cmd)
findCaseSpec sg =
  find (matchesCase sg)

runEntryFn :: ReactEnv -> ctx -> EntryFn ctx hf cmd -> Either ReactErr (ReactRez ctx hf cmd)
runEntryFn env ctx entryFn =
  entryFn env ctx

runCaseFn :: ReactEnv -> sg -> ctx -> CaseFn sg ctx hf cmd -> Either ReactErr (ReactRez ctx hf cmd)
runCaseFn env sg ctx caseFn =
  caseFn env sg ctx

runCaseSpec :: ReactEnv -> sg -> ctx -> CaseSpec sg ctx hf cmd -> Either ReactErr (ReactRez ctx hf cmd)
runCaseSpec env sg ctx spec =
  spec.react env sg ctx

normalizeKey :: Text -> Maybe Text
normalizeKey raw =
  let txt = T.strip raw
  in if T.null txt then Nothing else Just txt