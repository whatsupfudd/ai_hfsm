{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Hfsm.Core.Name
  ( NameKind(..)
  , NameErr(..)
  , MachineName
  , StateName
  , RegionName
  , CaseName
  , HandoffName
  , JoinName
  , TimerName
  , BreakName
  , maxNameLen
  , machineNameText
  , stateNameText
  , regionNameText
  , caseNameText
  , handoffNameText
  , joinNameText
  , timerNameText
  , breakNameText
  , mkMachineName
  , mkStateName
  , mkRegionName
  , mkCaseName
  , mkHandoffName
  , mkJoinName
  , mkTimerName
  , mkBreakName
  , validateMachineName
  , validateStateName
  , validateRegionName
  , validateCaseName
  , validateHandoffName
  , validateJoinName
  , validateTimerName
  , validateBreakName
  , renderNameErr
  ) where

import Control.DeepSeq (NFData)

import Data.Char (isControl)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (FromJSON(..), ToJSON, ToJSONKey, withText, FromJSONKeyFunction(..))
import Data.Aeson.Types (FromJSONKey(..))


data NameKind =
    MachineNk
  | StateNk
  | RegionNk
  | CaseNk
  | HandoffNk
  | JoinNk
  | TimerNk
  | BreakNk
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data NameErr =
    EmptyEr NameKind
  | TooLongEr NameKind Int Int
  | ControlCharEr NameKind Int Char
  deriving stock (Eq, Ord, Show, Read)

newtype MachineName = MachineName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype StateName = StateName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype RegionName = RegionName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype CaseName = CaseName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype HandoffName = HandoffName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype JoinName = JoinName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype TimerName = TimerName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype BreakName = BreakName Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

maxNameLen :: Int
maxNameLen = 200

machineNameText :: MachineName -> Text
machineNameText (MachineName x) = x

stateNameText :: StateName -> Text
stateNameText (StateName x) = x

regionNameText :: RegionName -> Text
regionNameText (RegionName x) = x

caseNameText :: CaseName -> Text
caseNameText (CaseName x) = x

handoffNameText :: HandoffName -> Text
handoffNameText (HandoffName x) = x

joinNameText :: JoinName -> Text
joinNameText (JoinName x) = x

timerNameText :: TimerName -> Text
timerNameText (TimerName x) = x

breakNameText :: BreakName -> Text
breakNameText (BreakName x) = x

mkMachineName :: Text -> Either NameErr MachineName
mkMachineName = mkName MachineNk MachineName

mkStateName :: Text -> Either NameErr StateName
mkStateName = mkName StateNk StateName

mkRegionName :: Text -> Either NameErr RegionName
mkRegionName = mkName RegionNk RegionName

mkCaseName :: Text -> Either NameErr CaseName
mkCaseName = mkName CaseNk CaseName

mkHandoffName :: Text -> Either NameErr HandoffName
mkHandoffName = mkName HandoffNk HandoffName

mkJoinName :: Text -> Either NameErr JoinName
mkJoinName = mkName JoinNk JoinName

mkTimerName :: Text -> Either NameErr TimerName
mkTimerName = mkName TimerNk TimerName

mkBreakName :: Text -> Either NameErr BreakName
mkBreakName = mkName BreakNk BreakName

validateMachineName :: Text -> Either NameErr ()
validateMachineName = fmap (const ()) . mkMachineName

validateStateName :: Text -> Either NameErr ()
validateStateName = fmap (const ()) . mkStateName

validateRegionName :: Text -> Either NameErr ()
validateRegionName = fmap (const ()) . mkRegionName

validateCaseName :: Text -> Either NameErr ()
validateCaseName = fmap (const ()) . mkCaseName

validateHandoffName :: Text -> Either NameErr ()
validateHandoffName = fmap (const ()) . mkHandoffName

validateJoinName :: Text -> Either NameErr ()
validateJoinName = fmap (const ()) . mkJoinName

validateTimerName :: Text -> Either NameErr ()
validateTimerName = fmap (const ()) . mkTimerName

validateBreakName :: Text -> Either NameErr ()
validateBreakName = fmap (const ()) . mkBreakName

renderNameErr :: NameErr -> Text
renderNameErr err =
  case err of
    EmptyEr kind ->
      kindText kind <> " name cannot be empty"

    TooLongEr kind lim actual ->
      kindText kind <> " name is too long; max=" <> tshow lim <> ", actual=" <> tshow actual

    ControlCharEr kind ix ch ->
      kindText kind <> " name contains a control character at index " <> tshow ix <> ": " <> T.singleton ch

instance FromJSON MachineName where
  parseJSON = withText "MachineName" $ either (fail . T.unpack . renderNameErr) pure . mkMachineName

instance FromJSON StateName where
  parseJSON = withText "StateName" $ either (fail . T.unpack . renderNameErr) pure . mkStateName

instance FromJSON RegionName where
  parseJSON = withText "RegionName" $ either (fail . T.unpack . renderNameErr) pure . mkRegionName

instance FromJSON CaseName where
  parseJSON = withText "CaseName" $ either (fail . T.unpack . renderNameErr) pure . mkCaseName

instance FromJSON HandoffName where
  parseJSON = withText "HandoffName" $ either (fail . T.unpack . renderNameErr) pure . mkHandoffName

instance FromJSON JoinName where
  parseJSON = withText "JoinName" $ either (fail . T.unpack . renderNameErr) pure . mkJoinName

instance FromJSON TimerName where
  parseJSON = withText "TimerName" $ either (fail . T.unpack . renderNameErr) pure . mkTimerName

instance FromJSON BreakName where
  parseJSON = withText "BreakName" $ either (fail . T.unpack . renderNameErr) pure . mkBreakName

instance FromJSONKey MachineName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkMachineName

instance FromJSONKey StateName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkStateName

instance FromJSONKey RegionName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkRegionName

instance FromJSONKey CaseName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkCaseName

instance FromJSONKey HandoffName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkHandoffName

instance FromJSONKey JoinName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkJoinName

instance FromJSONKey TimerName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkTimerName

instance FromJSONKey BreakName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderNameErr) pure . mkBreakName

mkName :: NameKind -> (Text -> a) -> Text -> Either NameErr a
mkName kind wrap raw = do
  let txt = T.strip raw
  ensureNotEmpty kind txt
  ensureLen kind txt
  ensureNoControl kind txt
  pure (wrap txt)

ensureNotEmpty :: NameKind -> Text -> Either NameErr ()
ensureNotEmpty kind txt
  | T.null txt = Left (EmptyEr kind)
  | otherwise = Right ()

ensureLen :: NameKind -> Text -> Either NameErr ()
ensureLen kind txt =
  let len = T.length txt
  in if len > maxNameLen
       then Left (TooLongEr kind maxNameLen len)
       else Right ()

ensureNoControl :: NameKind -> Text -> Either NameErr ()
ensureNoControl kind txt =
  case firstControlChar txt of
    Nothing -> Right ()
    Just (ix, ch) -> Left (ControlCharEr kind ix ch)

firstControlChar :: Text -> Maybe (Int, Char)
firstControlChar txt =
  go 0 (T.unpack txt)
  where
    go !_ [] = Nothing
    go !ix (x : xs)
      | isControl x = Just (ix, x)
      | otherwise = go (ix + 1) xs

kindText :: NameKind -> Text
kindText kind =
  case kind of
    MachineNk -> "machine"
    StateNk -> "state"
    RegionNk -> "region"
    CaseNk -> "case"
    HandoffNk -> "handoff"
    JoinNk -> "join"
    TimerNk -> "timer"
    BreakNk -> "break"

tshow :: Show a => a -> Text
tshow = T.pack . show