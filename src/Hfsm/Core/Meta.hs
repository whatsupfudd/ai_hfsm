{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Core.Meta
  ( MetaSpec(..)
  , SourceSpan
  , SpanErr(..)
  , emptyMetaSpec
  , mkSourceSpan
  , pointSourceSpan
  , validateSourceSpan
  , mergeMetaSpec
  , addTag
  , dropTag
  , hasTag
  , addSpan
  , renderSpanErr
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (void)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)


data MetaSpec = MetaSpec
  { tags :: Set Text
  , note :: Maybe Text
  , owner :: Maybe Text
  , rank :: Maybe Int
  , spans :: [SourceSpan]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data SpanErr =
    EmptyFileEr
  | NonPositiveLineEr Int
  | NonPositiveColEr Int
  | ReverseEr Int Int Int Int
  deriving stock (Eq, Ord, Show, Read)

data SourceSpan = SourceSpan
  { file :: FilePath
  , line :: Int
  , col :: Int
  , lineEnd :: Int
  , colEnd :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

emptyMetaSpec :: MetaSpec
emptyMetaSpec =
  MetaSpec
    { tags = S.empty
    , note = Nothing
    , owner = Nothing
    , rank = Nothing
    , spans = []
    }


mkSourceSpan :: FilePath -> Int -> Int -> Int -> Int -> Either SpanErr SourceSpan
mkSourceSpan rawFile rawLine rawCol rawLineEnd rawColEnd = do
  let file' = normalizeFile rawFile
  ensureFile file'
  ensureLine rawLine
  ensureLine rawLineEnd
  ensureCol rawCol
  ensureCol rawColEnd
  ensureOrder rawLine rawCol rawLineEnd rawColEnd
  pure SourceSpan
    { file = file'
    , line = rawLine
    , col = rawCol
    , lineEnd = rawLineEnd
    , colEnd = rawColEnd
    }

pointSourceSpan :: FilePath -> Int -> Int -> Either SpanErr SourceSpan
pointSourceSpan file' line' col' =
  mkSourceSpan file' line' col' line' col'

validateSourceSpan :: SourceSpan -> Either SpanErr ()
validateSourceSpan span =
  void $ mkSourceSpan span.file span.line span.col span.lineEnd span.colEnd

mergeMetaSpec :: MetaSpec -> MetaSpec -> MetaSpec
mergeMetaSpec a b =
  MetaSpec
    { tags = a.tags <> b.tags
    , note = b.note <|> a.note
    , owner = b.owner <|> a.owner
    , rank = b.rank <|> a.rank
    , spans = a.spans <> b.spans
    }

addTag :: Text -> MetaSpec -> MetaSpec
addTag raw meta =
  let tag = T.strip raw
  in if T.null tag
       then meta
       else meta { tags = S.insert tag meta.tags }

dropTag :: Text -> MetaSpec -> MetaSpec
dropTag raw meta =
  let tag = T.strip raw
  in meta { tags = S.delete tag meta.tags }

hasTag :: Text -> MetaSpec -> Bool
hasTag raw meta =
  let tag = T.strip raw
  in S.member tag meta.tags

addSpan :: SourceSpan -> MetaSpec -> MetaSpec
addSpan span meta =
  meta { spans = meta.spans <> [span] }

renderSpanErr :: SpanErr -> Text
renderSpanErr err =
  case err of
    EmptyFileEr ->
      "source span file cannot be empty"

    NonPositiveLineEr n ->
      "source span line must be positive: " <> tshow n

    NonPositiveColEr n ->
      "source span column must be positive: " <> tshow n

    ReverseEr lineStart colStart lineStop colStop ->
      "source span end cannot precede its start: (" <>
      tshow lineStart <> "," <> tshow colStart <> ") -> (" <>
      tshow lineStop <> "," <> tshow colStop <> ")"

normalizeFile :: FilePath -> FilePath
normalizeFile = T.unpack . T.strip . T.pack

ensureFile :: FilePath -> Either SpanErr ()
ensureFile file'
  | null file' = Left EmptyFileEr
  | otherwise = Right ()

ensureLine :: Int -> Either SpanErr ()
ensureLine n
  | n <= 0 = Left (NonPositiveLineEr n)
  | otherwise = Right ()

ensureCol :: Int -> Either SpanErr ()
ensureCol n
  | n <= 0 = Left (NonPositiveColEr n)
  | otherwise = Right ()

ensureOrder :: Int -> Int -> Int -> Int -> Either SpanErr ()
ensureOrder lineStart colStart lineStop colStop
  | lineStop < lineStart = Left (ReverseEr lineStart colStart lineStop colStop)
  | lineStop == lineStart && colStop < colStart = Left (ReverseEr lineStart colStart lineStop colStop)
  | otherwise = Right ()

tshow :: Show a => a -> Text
tshow = T.pack . show