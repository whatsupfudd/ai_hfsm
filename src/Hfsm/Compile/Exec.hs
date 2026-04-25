{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Compile.Exec
  ( ExecTable(..)
  , EntryExec(..)
  , CaseExec(..)
  , emptyExecTable
  , nullExecTable
  , entryCount
  , caseCount
  , mkEntryExec
  , mkCaseExec
  , singletonEntryExec
  , singletonCaseExec
  , insertEntryExec
  , insertCaseExec
  , insertEntryFn
  , insertCaseSpec
  , fromEntryExecs
  , fromCaseExecs
  , fromExecs
  , memberEntryRef
  , memberCaseRef
  , lookupEntryExec
  , lookupCaseExec
  , lookupEntryFn
  , lookupCaseFn
  , entryRefs
  , caseRefs
  , entryExecs
  , caseExecs
  , entryExecsForState
  , caseExecsForState
  , matchingCases
  , findMatchingCase
  , runEntryExec
  , runCaseExec
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe (listToMaybe, mapMaybe)

import Hfsm.Core.Ref (CaseRef, StateRef, caseRefWord32)
import Hfsm.Spec.Case
  ( CaseFn
  , CaseSpec(..)
  , EntryFn
  , ReactEnv
  , ReactErr
  , ReactRez
  , matchesCase
  , runCaseSpec
  , runEntryFn
  )

data ExecTable sg ctx hf cmd = ExecTable
  { entry :: IntMap (EntryExec ctx hf cmd)
  , cases :: IntMap (CaseExec sg ctx hf cmd)
  }

data EntryExec ctx hf cmd = EntryExec
  { state :: StateRef
  , caseRef :: CaseRef
  , react :: EntryFn ctx hf cmd
  }

data CaseExec sg ctx hf cmd = CaseExec
  { state :: StateRef
  , caseRef :: CaseRef
  , spec :: CaseSpec sg ctx hf cmd
  }

instance Show (ExecTable sg ctx hf cmd) where
  showsPrec d table =
    showParen (d > 10) $
      showString "ExecTable {entryCount = "
      . shows (entryCount table)
      . showString ", caseCount = "
      . shows (caseCount table)
      . showString "}"

instance Show (EntryExec ctx hf cmd) where
  showsPrec d entryExec =
    showParen (d > 10) $
      showString "EntryExec {state = "
      . shows entryExec.state
      . showString ", caseRef = "
      . shows entryExec.caseRef
      . showString "}"

instance Show (CaseExec sg ctx hf cmd) where
  showsPrec d caseExec =
    showParen (d > 10) $
      showString "CaseExec {state = "
      . shows caseExec.state
      . showString ", caseRef = "
      . shows caseExec.caseRef
      . showString ", name = "
      . shows caseExec.spec.name
      . showString "}"

emptyExecTable :: ExecTable sg ctx hf cmd
emptyExecTable =
  ExecTable
    { entry = IM.empty
    , cases = IM.empty
    }

nullExecTable :: ExecTable sg ctx hf cmd -> Bool
nullExecTable table =
  IM.null table.entry && IM.null table.cases

entryCount :: ExecTable sg ctx hf cmd -> Int
entryCount table =
  IM.size table.entry

caseCount :: ExecTable sg ctx hf cmd -> Int
caseCount table =
  IM.size table.cases

mkEntryExec :: StateRef -> CaseRef -> EntryFn ctx hf cmd -> EntryExec ctx hf cmd
mkEntryExec stateRef caseRef react =
  EntryExec
    { state = stateRef
    , caseRef = caseRef
    , react = react
    }

mkCaseExec :: StateRef -> CaseRef -> CaseSpec sg ctx hf cmd -> CaseExec sg ctx hf cmd
mkCaseExec stateRef caseRef spec =
  CaseExec
    { state = stateRef
    , caseRef = caseRef
    , spec = spec
    }

singletonEntryExec :: EntryExec ctx hf cmd -> ExecTable sg ctx hf cmd
singletonEntryExec entryExec =
  insertEntryExec entryExec emptyExecTable

singletonCaseExec :: CaseExec sg ctx hf cmd -> ExecTable sg ctx hf cmd
singletonCaseExec caseExec =
  insertCaseExec caseExec emptyExecTable

insertEntryExec :: EntryExec ctx hf cmd -> ExecTable sg ctx hf cmd -> ExecTable sg ctx hf cmd
insertEntryExec entryExec table =
  table
    { entry = IM.insert (caseRefKey entryExec.caseRef) entryExec table.entry
    }

insertCaseExec :: CaseExec sg ctx hf cmd -> ExecTable sg ctx hf cmd -> ExecTable sg ctx hf cmd
insertCaseExec caseExec table =
  table
    { cases = IM.insert (caseRefKey caseExec.caseRef) caseExec table.cases
    }

insertEntryFn :: StateRef -> CaseRef -> EntryFn ctx hf cmd -> ExecTable sg ctx hf cmd -> ExecTable sg ctx hf cmd
insertEntryFn stateRef caseRef react =
  insertEntryExec (mkEntryExec stateRef caseRef react)

insertCaseSpec :: StateRef -> CaseRef -> CaseSpec sg ctx hf cmd -> ExecTable sg ctx hf cmd -> ExecTable sg ctx hf cmd
insertCaseSpec stateRef caseRef spec =
  insertCaseExec (mkCaseExec stateRef caseRef spec)

fromEntryExecs :: [EntryExec ctx hf cmd] -> ExecTable sg ctx hf cmd
fromEntryExecs =
  foldl' (flip insertEntryExec) emptyExecTable

fromCaseExecs :: [CaseExec sg ctx hf cmd] -> ExecTable sg ctx hf cmd
fromCaseExecs =
  foldl' (flip insertCaseExec) emptyExecTable

fromExecs :: [EntryExec ctx hf cmd] -> [CaseExec sg ctx hf cmd] -> ExecTable sg ctx hf cmd
fromExecs entryExecs' caseExecs' =
  foldl' (flip insertCaseExec) (fromEntryExecs entryExecs') caseExecs'

memberEntryRef :: CaseRef -> ExecTable sg ctx hf cmd -> Bool
memberEntryRef caseRef table =
  IM.member (caseRefKey caseRef) table.entry

memberCaseRef :: CaseRef -> ExecTable sg ctx hf cmd -> Bool
memberCaseRef caseRef table =
  IM.member (caseRefKey caseRef) table.cases

lookupEntryExec :: CaseRef -> ExecTable sg ctx hf cmd -> Maybe (EntryExec ctx hf cmd)
lookupEntryExec caseRef table =
  IM.lookup (caseRefKey caseRef) table.entry

lookupCaseExec :: CaseRef -> ExecTable sg ctx hf cmd -> Maybe (CaseExec sg ctx hf cmd)
lookupCaseExec caseRef table =
  IM.lookup (caseRefKey caseRef) table.cases

lookupEntryFn :: CaseRef -> ExecTable sg ctx hf cmd -> Maybe (EntryFn ctx hf cmd)
lookupEntryFn caseRef table = do
  entryExec <- lookupEntryExec caseRef table
  pure entryExec.react

lookupCaseFn :: CaseRef -> ExecTable sg ctx hf cmd -> Maybe (CaseFn sg ctx hf cmd)
lookupCaseFn caseRef table = do
  caseExec <- lookupCaseExec caseRef table
  pure caseExec.spec.react

entryRefs :: ExecTable sg ctx hf cmd -> [CaseRef]
entryRefs table =
  fmap (\entryExec -> entryExec.caseRef) (IM.elems table.entry)

caseRefs :: ExecTable sg ctx hf cmd -> [CaseRef]
caseRefs table =
  fmap (\caseExec -> caseExec.caseRef) (IM.elems table.cases)

entryExecs :: ExecTable sg ctx hf cmd -> [EntryExec ctx hf cmd]
entryExecs table =
  IM.elems table.entry

caseExecs :: ExecTable sg ctx hf cmd -> [CaseExec sg ctx hf cmd]
caseExecs table =
  IM.elems table.cases

entryExecsForState :: StateRef -> ExecTable sg ctx hf cmd -> [EntryExec ctx hf cmd]
entryExecsForState stateRef table =
  filter (\entryExec -> entryExec.state == stateRef) (entryExecs table)

caseExecsForState :: StateRef -> ExecTable sg ctx hf cmd -> [CaseExec sg ctx hf cmd]
caseExecsForState stateRef table =
  filter (\caseExec -> caseExec.state == stateRef) (caseExecs table)

matchingCases :: sg -> [CaseRef] -> ExecTable sg ctx hf cmd -> [CaseExec sg ctx hf cmd]
matchingCases sg caseRefs' table =
  mapMaybe matchOne caseRefs'
  where
    matchOne caseRef = do
      caseExec <- lookupCaseExec caseRef table
      if matchesCase sg caseExec.spec
        then Just caseExec
        else Nothing

findMatchingCase :: sg -> [CaseRef] -> ExecTable sg ctx hf cmd -> Maybe (CaseExec sg ctx hf cmd)
findMatchingCase sg caseRefs' table =
  listToMaybe (matchingCases sg caseRefs' table)

runEntryExec :: ReactEnv -> ctx -> EntryExec ctx hf cmd -> Either ReactErr (ReactRez ctx hf cmd)
runEntryExec env ctx entryExec =
  runEntryFn env ctx entryExec.react

runCaseExec :: ReactEnv -> sg -> ctx -> CaseExec sg ctx hf cmd -> Either ReactErr (ReactRez ctx hf cmd)
runCaseExec env sg ctx caseExec =
  runCaseSpec env sg ctx caseExec.spec

caseRefKey :: CaseRef -> Int
caseRefKey =
  fromIntegral . caseRefWord32