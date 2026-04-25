{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Hfsm.Core.Digest
  ( DigestErr(..)
  , SpecDigest
  , GraphDigest
  , digestLen
  , specDigestText
  , graphDigestText
  , mkSpecDigest
  , mkGraphDigest
  , validateSpecDigest
  , validateGraphDigest
  , renderDigestErr
  ) where

import Control.DeepSeq (NFData)

import Data.Char (isHexDigit, toLower)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (FromJSON(..), ToJSON, ToJSONKey, withText, FromJSONKeyFunction(..))
import Data.Aeson.Types (FromJSONKey(..))

data DigestErr =
    EmptyEr
  | WrongLenEr Int Int
  | InvalidCharEr Int Char
  deriving stock (Eq, Ord, Show, Read)

newtype SpecDigest = SpecDigest Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

newtype GraphDigest = GraphDigest Text
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey)

digestLen :: Int
digestLen = 64

specDigestText :: SpecDigest -> Text
specDigestText (SpecDigest x) = x

graphDigestText :: GraphDigest -> Text
graphDigestText (GraphDigest x) = x

mkSpecDigest :: Text -> Either DigestErr SpecDigest
mkSpecDigest = mkDigest SpecDigest

mkGraphDigest :: Text -> Either DigestErr GraphDigest
mkGraphDigest = mkDigest GraphDigest

validateSpecDigest :: Text -> Either DigestErr ()
validateSpecDigest = fmap (const ()) . mkSpecDigest

validateGraphDigest :: Text -> Either DigestErr ()
validateGraphDigest = fmap (const ()) . mkGraphDigest

renderDigestErr :: DigestErr -> Text
renderDigestErr err =
  case err of
    EmptyEr ->
      "digest cannot be empty"

    WrongLenEr expected actual ->
      "digest has the wrong length; expected=" <> tshow expected <> ", actual=" <> tshow actual

    InvalidCharEr ix ch ->
      "digest contains an invalid hexadecimal character at index " <> tshow ix <> ": " <> T.singleton ch

instance FromJSON SpecDigest where
  parseJSON = withText "SpecDigest" $ either (fail . T.unpack . renderDigestErr) pure . mkSpecDigest

instance FromJSON GraphDigest where
  parseJSON = withText "GraphDigest" $ either (fail . T.unpack . renderDigestErr) pure . mkGraphDigest

instance FromJSONKey SpecDigest where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderDigestErr) pure . mkSpecDigest

instance FromJSONKey GraphDigest where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack . renderDigestErr) pure . mkGraphDigest

mkDigest :: (Text -> a) -> Text -> Either DigestErr a
mkDigest wrap raw = do
  let txt = normalizeDigest raw
  ensureNotEmpty txt
  ensureLen txt
  ensureHex txt
  pure (wrap txt)

normalizeDigest :: Text -> Text
normalizeDigest = T.toLower . T.strip

ensureNotEmpty :: Text -> Either DigestErr ()
ensureNotEmpty txt
  | T.null txt = Left EmptyEr
  | otherwise = Right ()

ensureLen :: Text -> Either DigestErr ()
ensureLen txt =
  let len = T.length txt
  in if len /= digestLen
       then Left (WrongLenEr digestLen len)
       else Right ()

ensureHex :: Text -> Either DigestErr ()
ensureHex txt =
  case firstInvalidChar txt of
    Nothing -> Right ()
    Just (ix, ch) -> Left (InvalidCharEr ix ch)

firstInvalidChar :: Text -> Maybe (Int, Char)
firstInvalidChar txt =
  go 0 (T.unpack txt)
  where
    go !_ [] = Nothing
    go !ix (x : xs)
      | validDigestChar x = go (ix + 1) xs
      | otherwise = Just (ix, x)

validDigestChar :: Char -> Bool
validDigestChar ch = isHexDigit ch

tshow :: Show a => a -> Text
tshow = T.pack . show