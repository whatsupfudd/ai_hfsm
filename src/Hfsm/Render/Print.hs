module Hfsm.Render.Print
  ( ppValue
  , ppValueCompact
  , ppStoreView
  , ppSection
  , ppSubsection
  , ppKeyValue
  , ppLines
  , printText
  , printValue
  , printStoreView
  ) where

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Aeson (KeyValue((.=)), Value(..), object)
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.KeyMap (KeyMap)


ppValue :: Value -> Text
ppValue =
  ppValueAt 0

ppValueCompact :: Value -> Text
ppValueCompact value =
  case value of
    Null ->
      "null"

    Bool x ->
      if x then "true" else "false"

    Number x ->
      ppScientific x

    String x ->
      quote x

    Array xs ->
      "[" <> T.intercalate ", " (fmap ppValueCompact (V.toList xs)) <> "]"

    Object obj ->
      let
        fields =
          fmap
            (\(k, v) -> K.toText k <> ": " <> ppValueCompact v)
            (KM.toList obj)
      in
      "{ " <> T.intercalate ", " fields <> " }"

ppStoreView :: Value -> Text
ppStoreView value =
  case value of
    Object obj ->
      T.intercalate "\n\n" $
        filter (not . T.null)
          [ ppStoreSingleton "Instance" $ fromMaybe emptyValue ( lookupObj "instance" obj)
          , ppStoreSingleton "Snapshot" $ fromMaybe emptyValue ( lookupObj "snapshot" obj)
          , ppStoreArray "Signals" $ fromMaybe emptyValue ( lookupObj "signals" obj)
          , ppStoreArray "Steps" $ fromMaybe emptyValue ( lookupObj "steps" obj)
          , ppStoreArray "Outbox" $ fromMaybe emptyValue ( lookupObj "outbox" obj)
          , ppStoreArray "Children" $ fromMaybe emptyValue ( lookupObj "children" obj)
          , ppStoreArray "Breaks" $ fromMaybe emptyValue ( lookupObj "breaks" obj)
          , ppStoreProjections $ fromMaybe emptyValue ( lookupObj "projections" obj)
          ]

    _ ->
      ppSection "Store view" (ppValue value)

ppSection :: Text -> Text -> Text
ppSection title body =
  title <> "\n" <> T.replicate (T.length title) "=" <> "\n" <> body

ppSubsection :: Text -> Text -> Text
ppSubsection title body =
  title <> "\n" <> T.replicate (T.length title) "-" <> "\n" <> body

ppKeyValue :: Text -> Text -> Text
ppKeyValue key value =
  key <> ": " <> value

ppLines :: [Text] -> Text
ppLines =
  T.intercalate "\n"

printText :: Text -> IO ()
printText =
  TIO.putStrLn

printValue :: Value -> IO ()
printValue =
  printText . ppValue

printStoreView :: Value -> IO ()
printStoreView =
  printText . ppStoreView

ppStoreSingleton :: Text -> Value -> Text
ppStoreSingleton title value =
  case value of
    Null ->
      ppSubsection title "  <none>"

    Object obj ->
      ppSubsection title (ppObjectFields 2 obj)

    _ ->
      ppSubsection title ("  " <> ppValueCompact value)

ppStoreArray :: Text -> Value -> Text
ppStoreArray title value =
  case value of
    Array xs
      | V.null xs ->
          ppSubsection title "  <none>"

      | otherwise ->
          ppSubsection title $
            T.intercalate "\n" $
              fmap ppArrayItem $
                zip [1 :: Int ..] (V.toList xs)

    _ ->
      ppSubsection title ("  " <> ppValueCompact value)

emptyValue :: Value
emptyValue =
  Array V.empty

ppStoreProjections :: Value -> Text
ppStoreProjections value =
  case value of
    Object obj ->
      ppSubsection "Projections" $
        T.intercalate "\n\n" $
          filter (not . T.null)
            [ ppProjectionArray "Active" $ fromMaybe emptyValue (lookupObj "active" obj)
            , ppProjectionArray "Trace" $ fromMaybe emptyValue (lookupObj "trace" obj)
            , ppProjectionArray "Queue" $ fromMaybe emptyValue (lookupObj "queue" obj)
            , ppProjectionArray "Tree" $ fromMaybe emptyValue (lookupObj "tree" obj)
            ]

    _ ->
      ppSubsection "Projections" ("  " <> ppValueCompact value)

ppProjectionArray :: Text -> Value -> Text
ppProjectionArray title value =
  case value of
    Array xs
      | V.null xs ->
          indent 2 (title <> ": <none>")

      | otherwise ->
          indent 2 title <> "\n" <>
          T.intercalate "\n" (fmap ppArrayItem (zip [1 :: Int ..] (V.toList xs)))

    _ ->
      indent 2 (title <> ": " <> ppValueCompact value)

ppArrayItem :: (Int, Value) -> Text
ppArrayItem (ix, value) =
  case value of
    Object obj ->
      indent 2 ("#" <> tshow ix) <> "\n" <> ppObjectFields 4 obj

    _ ->
      indent 2 ("#" <> tshow ix <> " " <> ppValueCompact value)

ppValueAt :: Int -> Value -> Text
ppValueAt level value =
  case value of
    Null ->
      indent level "null"

    Bool x ->
      indent level $
        if x then "true" else "false"

    Number x ->
      indent level (ppScientific x)

    String x ->
      indent level (quote x)

    Array xs ->
      ppArrayAt level xs

    Object obj ->
      ppObjectAt level obj

ppArrayAt :: Int -> Vector Value -> Text
ppArrayAt level xs
  | V.null xs =
      indent level "[]"

  | otherwise =
      indent level "[" <> "\n" <>
      T.intercalate ",\n" (fmap (ppValueAt (level + 2)) (V.toList xs)) <>
      "\n" <> indent level "]"

ppObjectAt :: Int -> KeyMap Value -> Text
ppObjectAt level obj
  | KM.null obj =
      indent level "{}"

  | otherwise =
      indent level "{" <> "\n" <>
      T.intercalate ",\n" (fmap ppField (KM.toList obj)) <>
      "\n" <> indent level "}"
  where
    ppField (key, fieldValue) =
      indent (level + 2) (K.toText key <> ": ") <>
      case fieldValue of
        Null -> "null"
        Bool x -> if x then "true" else "false"
        Number x -> ppScientific x
        String x -> quote x
        Array xs -> "\n" <> ppArrayAt (level + 4) xs
        Object nested -> "\n" <> ppObjectAt (level + 4) nested

ppObjectFields :: Int -> KeyMap Value -> Text
ppObjectFields level obj =
  T.intercalate "\n" $
    fmap ppField $
      reorderFields defaultFieldOrder $
        KM.toList obj
  where
    ppField (key, fieldValue) =
      indent level (K.toText key <> ": " <> ppValueCompact fieldValue)

reorderFields :: [Text] -> [(K.Key, Value)] -> [(K.Key, Value)]
reorderFields preferred fields =
  let
    byKey =
      KM.fromList fields

    preferredFields =
      concatMap
        (\name ->
          case KM.lookup (K.fromText name) byKey of
            Nothing -> []
            Just value -> [(K.fromText name, value)]
        )
        preferred

    preferredSet =
      KM.fromList preferredFields

    rest =
      filter
        (\(key, _) -> not (KM.member key preferredSet))
        fields
  in
  preferredFields <> rest

lookupObj :: Text -> KeyMap Value -> Maybe Value
lookupObj key =
  KM.lookup (K.fromText key)

defaultFieldOrder :: [Text]
defaultFieldOrder =
  [ "uid"
  , "uuid"
  , "machine"
  , "version"
  , "digest"
  , "status"
  , "state"
  , "path"
  , "snap"
  , "inst"
  , "signal"
  , "kind"
  , "from"
  , "to"
  , "caseRef"
  , "route"
  , "payload"
  , "ctx"
  , "wait"
  , "child"
  , "cause"
  , "lease"
  , "step"
  , "parent"
  , "key"
  , "join"
  , "note"
  , "createdAt"
  , "updatedAt"
  ]

indent :: Int -> Text -> Text
indent level txt =
  T.replicate level " " <> txt

quote :: Text -> Text
quote txt =
  "\"" <> txt <> "\""

ppScientific :: Scientific -> Text
ppScientific x =
  T.pack (Sci.formatScientific Sci.Generic Nothing x)

tshow :: Show a => a -> Text
tshow =
  T.pack . show