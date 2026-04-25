{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Hfsm.Core.Version
  ( MachineVersion
  , VersionPolicy(..)
  , VersionErr(..)
  , maxVersionLen
  , machineVersionText
  , mkMachineVersion
  , validateMachineVersion
  , versionSeries
  , sameSeries
  , acceptsVersion
  , needsMigratePlan
  , renderVersionPolicy
  , renderVersionErr
  ) where

import Control.DeepSeq (NFData)

import Data.Char (isAlphaNum, isControl)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (FromJSON(..), ToJSON, ToJSONKey, withText, FromJSONKeyFunction(..))
import Data.Aeson.Types (FromJSONKey(..))

newtype MachineVersion = MachineVersion Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

data VersionPolicy =
    ExactVp
  | CompatibleVp
  | MigratableVp
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data VersionErr =
    EmptyEr
  | TooLongEr Int Int
  | InvalidCharEr Int Char
  | EmptySegmentEr
  deriving stock (Eq, Ord, Show, Read)

maxVersionLen :: Int
maxVersionLen = 120

machineVersionText :: MachineVersion -> Text
machineVersionText (MachineVersion x) = x

mkMachineVersion :: Text -> Either VersionErr MachineVersion
mkMachineVersion raw = do
  let txt = T.strip raw
  ensureNotEmpty txt
  ensureLen txt
  ensureChars txt
  ensureSegments txt
  pure (MachineVersion txt)

validateMachineVersion :: Text -> Either VersionErr ()
validateMachineVersion = fmap (const ()) . mkMachineVersion

versionSeries :: MachineVersion -> Text
versionSeries (MachineVersion txt) =
  case T.splitOn "." txt of
    [] -> txt
    x : _ -> x

sameSeries :: MachineVersion -> MachineVersion -> Bool
sameSeries a b = versionSeries a == versionSeries b

acceptsVersion :: VersionPolicy -> MachineVersion -> MachineVersion -> Bool
acceptsVersion policy expected actual =
  case policy of
    ExactVp -> actual == expected
    CompatibleVp -> sameSeries expected actual
    MigratableVp -> True

needsMigratePlan :: VersionPolicy -> Bool
needsMigratePlan policy =
  case policy of
    MigratableVp -> True
    ExactVp -> False
    CompatibleVp -> False

renderVersionPolicy :: VersionPolicy -> Text
renderVersionPolicy policy =
  case policy of
    ExactVp -> "exact"
    CompatibleVp -> "compatible"
    MigratableVp -> "migratable"

renderVersionErr :: VersionErr -> Text
renderVersionErr err =
  case err of
    EmptyEr ->
      "machine version cannot be empty"

    TooLongEr lim actual ->
      "machine version is too long; max=" <> tshow lim <> ", actual=" <> tshow actual

    InvalidCharEr ix ch ->
      "machine version contains an invalid character at index " <> tshow ix <> ": " <> T.singleton ch

    EmptySegmentEr ->
      "machine version contains an empty dot-separated segment"

instance FromJSON MachineVersion where
  parseJSON = withText "MachineVersion" $ either (fail . T.unpack . renderVersionErr) pure . mkMachineVersion

instance FromJSONKey MachineVersion where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderVersionErr) pure . mkMachineVersion

ensureNotEmpty :: Text -> Either VersionErr ()
ensureNotEmpty txt
  | T.null txt = Left EmptyEr
  | otherwise = Right ()

ensureLen :: Text -> Either VersionErr ()
ensureLen txt =
  let
    len = T.length txt
  in
  if len > maxVersionLen
       then Left (TooLongEr maxVersionLen len)
       else Right ()

ensureChars :: Text -> Either VersionErr ()
ensureChars txt =
  case firstInvalidChar txt of
    Nothing -> Right ()
    Just (ix, ch) -> Left (InvalidCharEr ix ch)

ensureSegments :: Text -> Either VersionErr ()
ensureSegments txt
  | any T.null (T.splitOn "." txt) = Left EmptySegmentEr
  | otherwise = Right ()

firstInvalidChar :: Text -> Maybe (Int, Char)
firstInvalidChar txt =
  go 0 (T.unpack txt)
  where
    go !_ [] = Nothing
    go !ix (x : xs)
      | validVersionChar x = go (ix + 1) xs
      | otherwise = Just (ix, x)


validVersionChar :: Char -> Bool
validVersionChar ch =
  isAlphaNum ch || ch == '.' || ch == '-' || ch == '_' || ch == '+'


tshow :: Show a => a -> Text
tshow = T.pack . show