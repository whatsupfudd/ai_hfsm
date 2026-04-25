{-# LANGUAGE DerivingStrategies #-}
module Hfsm.Core.Error
  ( ErrKind(..)
  , HfsmErr(..)
  , errKind
  , compileErr
  , validateErr
  , engineErr
  , storeErr
  , replayErr
  , migrateErr
  , codecErr
  , addContext
  , renderErrKind
  , renderHfsmErr
  ) where

import Data.Text (Text)

import Hfsm.Core.Codec (CodecErr, renderCodecErr)

data ErrKind =
    CompileEk
  | ValidateEk
  | EngineEk
  | StoreEk
  | ReplayEk
  | MigrateEk
  | CodecEk
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data HfsmErr =
    CompileEr Text
  | ValidateEr Text
  | EngineEr Text
  | StoreEr Text
  | ReplayEr Text
  | MigrateEr Text
  | CodecEr CodecErr
  deriving stock (Eq, Ord, Show, Read)


errKind :: HfsmErr -> ErrKind
errKind err =
  case err of
    CompileEr _ -> CompileEk
    ValidateEr _ -> ValidateEk
    EngineEr _ -> EngineEk
    StoreEr _ -> StoreEk
    ReplayEr _ -> ReplayEk
    MigrateEr _ -> MigrateEk
    CodecEr _ -> CodecEk

compileErr :: Text -> HfsmErr
compileErr = CompileEr

validateErr :: Text -> HfsmErr
validateErr = ValidateEr

engineErr :: Text -> HfsmErr
engineErr = EngineEr

storeErr :: Text -> HfsmErr
storeErr = StoreEr

replayErr :: Text -> HfsmErr
replayErr = ReplayEr

migrateErr :: Text -> HfsmErr
migrateErr = MigrateEr

codecErr :: CodecErr -> HfsmErr
codecErr = CodecEr

addContext :: Text -> HfsmErr -> HfsmErr
addContext ctx err =
  case err of
    CompileEr msg -> CompileEr (ctx <> ": " <> msg)
    ValidateEr msg -> ValidateEr (ctx <> ": " <> msg)
    EngineEr msg -> EngineEr (ctx <> ": " <> msg)
    StoreEr msg -> StoreEr (ctx <> ": " <> msg)
    ReplayEr msg -> ReplayEr (ctx <> ": " <> msg)
    MigrateEr msg -> MigrateEr (ctx <> ": " <> msg)
    CodecEr codec ->
      CompileEr (ctx <> ": " <> renderCodecErr codec)

renderErrKind :: ErrKind -> Text
renderErrKind kind =
  case kind of
    CompileEk -> "compile"
    ValidateEk -> "validate"
    EngineEk -> "engine"
    StoreEk -> "store"
    ReplayEk -> "replay"
    MigrateEk -> "migrate"
    CodecEk -> "codec"

renderHfsmErr :: HfsmErr -> Text
renderHfsmErr err =
  case err of
    CompileEr msg ->
      "compile error: " <> msg

    ValidateEr msg ->
      "validate error: " <> msg

    EngineEr msg ->
      "engine error: " <> msg

    StoreEr msg ->
      "store error: " <> msg

    ReplayEr msg ->
      "replay error: " <> msg

    MigrateEr msg ->
      "migrate error: " <> msg

    CodecEr codec ->
      renderCodecErr codec