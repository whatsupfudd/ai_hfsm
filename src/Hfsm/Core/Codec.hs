{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Hfsm.Core.Codec
  ( CodecErr(..)
  , Codec(..)
  , CodecSet(..)
  , mkCodec
  , mkCodecSet
  , jsonCodec
  , jsonCodecWithSchema
  , encodeWith
  , decodeWith
  , validateWith
  , setSchema
  , mapCodec
  , renderCodecErr
  ) where

import Control.Monad ((>=>))

import Data.Aeson (FromJSON, ToJSON, Value, toJSON)
import Data.Aeson.Types (Result(..), fromJSON)
import Data.Text (Text)
import qualified Data.Text as T

data CodecErr =
    DecodeEr Text
  | EncodeEr Text
  | SchemaEr Text
  deriving stock (Eq, Ord, Show, Read)

data Codec a = Codec
  { encode :: a -> Value
  , decode :: Value -> Either CodecErr a
  , schema :: Maybe Value
  }

data CodecSet ctx sg cmd = CodecSet
  { ctx :: Codec ctx
  , signal :: Codec sg
  , cmd :: Codec cmd
  }

mkCodec :: (a -> Value) -> (Value -> Either CodecErr a) -> Maybe Value -> Codec a
mkCodec encode decode schema =
  Codec
    { encode = encode
    , decode = decode
    , schema = schema
    }

mkCodecSet :: Codec ctx -> Codec sg -> Codec cmd -> CodecSet ctx sg cmd
mkCodecSet ctx signal cmd =
  CodecSet
    { ctx = ctx
    , signal = signal
    , cmd = cmd
    }

jsonCodec :: (ToJSON a, FromJSON a) => Codec a
jsonCodec =
  Codec
    { encode = toJSON
    , decode = decodeJsonValue
    , schema = Nothing
    }

jsonCodecWithSchema :: (ToJSON aT, FromJSON aT) => Value -> Codec aT
jsonCodecWithSchema schemaValue =
  jsonCodec { schema = Just schemaValue }

encodeWith :: Codec a -> a -> Value
encodeWith codec = codec.encode

decodeWith :: Codec a -> Value -> Either CodecErr a
decodeWith codec = codec.decode

validateWith :: Codec a -> Value -> Either CodecErr ()
validateWith codec value =
  fmap (const ()) (decodeWith codec value)

setSchema :: Maybe Value -> Codec a -> Codec a
setSchema schemaValue codec =
  codec { schema = schemaValue }

mapCodec :: (a -> Either CodecErr b) -> (b -> a) -> Codec a -> Codec b
mapCodec to from codec =
  Codec
    { encode = codec.encode . from
    , decode = codec.decode >=> to
    , schema = codec.schema
    }

renderCodecErr :: CodecErr -> Text
renderCodecErr err =
  case err of
    DecodeEr msg ->
      "codec decode error: " <> msg

    EncodeEr msg ->
      "codec encode error: " <> msg

    SchemaEr msg ->
      "codec schema error: " <> msg

decodeJsonValue :: FromJSON a => Value -> Either CodecErr a
decodeJsonValue value =
  case fromJSON value of
    Success x ->
      Right x

    Error msg ->
      Left (DecodeEr (T.pack msg))