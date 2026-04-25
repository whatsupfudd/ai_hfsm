{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hfsm.Core.Ref
  ( RefErr(..)
  , MachineRef
  , RegionRef
  , StateRef
  , CaseRef
  , RouteRef
  , JoinRef
  , TimerRef
  , BreakRef
  , machineRefWord32
  , regionRefWord32
  , stateRefWord32
  , caseRefWord32
  , routeRefWord32
  , joinRefWord32
  , timerRefWord32
  , breakRefWord32
  , mkMachineRef
  , mkRegionRef
  , mkStateRef
  , mkCaseRef
  , mkRouteRef
  , mkJoinRef
  , mkTimerRef
  , mkBreakRef
  , machineRefFromInt
  , regionRefFromInt
  , stateRefFromInt
  , caseRefFromInt
  , routeRefFromInt
  , joinRefFromInt
  , timerRefFromInt
  , breakRefFromInt
  , machineRefFromInteger
  , regionRefFromInteger
  , stateRefFromInteger
  , caseRefFromInteger
  , routeRefFromInteger
  , joinRefFromInteger
  , timerRefFromInteger
  , breakRefFromInteger
  , renderRefErr
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON, ToJSONKey, withScientific)
import Data.Aeson.Types (FromJSONKey(..))
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word32)
import Data.Aeson (FromJSONKeyFunction(..))

data RefErr =
    NegativeEr Integer
  | OverflowEr Integer
  | FractionalEr Scientific
  | InvalidTextEr Text
  deriving stock (Eq, Ord, Show, Read)

newtype MachineRef = MachineRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

newtype RegionRef = RegionRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

newtype StateRef = StateRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

newtype CaseRef = CaseRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

newtype RouteRef = RouteRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

newtype JoinRef = JoinRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

newtype TimerRef = TimerRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

newtype BreakRef = BreakRef Word32
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum, Bounded, NFData, Hashable, ToJSON, ToJSONKey)

machineRefWord32 :: MachineRef -> Word32
machineRefWord32 (MachineRef x) = x

regionRefWord32 :: RegionRef -> Word32
regionRefWord32 (RegionRef x) = x

stateRefWord32 :: StateRef -> Word32
stateRefWord32 (StateRef x) = x

caseRefWord32 :: CaseRef -> Word32
caseRefWord32 (CaseRef x) = x

routeRefWord32 :: RouteRef -> Word32
routeRefWord32 (RouteRef x) = x

joinRefWord32 :: JoinRef -> Word32
joinRefWord32 (JoinRef x) = x

timerRefWord32 :: TimerRef -> Word32
timerRefWord32 (TimerRef x) = x

breakRefWord32 :: BreakRef -> Word32
breakRefWord32 (BreakRef x) = x

mkMachineRef :: Word32 -> MachineRef
mkMachineRef = MachineRef

mkRegionRef :: Word32 -> RegionRef
mkRegionRef = RegionRef

mkStateRef :: Word32 -> StateRef
mkStateRef = StateRef

mkCaseRef :: Word32 -> CaseRef
mkCaseRef = CaseRef

mkRouteRef :: Word32 -> RouteRef
mkRouteRef = RouteRef

mkJoinRef :: Word32 -> JoinRef
mkJoinRef = JoinRef

mkTimerRef :: Word32 -> TimerRef
mkTimerRef = TimerRef

mkBreakRef :: Word32 -> BreakRef
mkBreakRef = BreakRef

machineRefFromInt :: Int -> Either RefErr MachineRef
machineRefFromInt = machineRefFromInteger . toInteger

regionRefFromInt :: Int -> Either RefErr RegionRef
regionRefFromInt = regionRefFromInteger . toInteger

stateRefFromInt :: Int -> Either RefErr StateRef
stateRefFromInt = stateRefFromInteger . toInteger

caseRefFromInt :: Int -> Either RefErr CaseRef
caseRefFromInt = caseRefFromInteger . toInteger

routeRefFromInt :: Int -> Either RefErr RouteRef
routeRefFromInt = routeRefFromInteger . toInteger

joinRefFromInt :: Int -> Either RefErr JoinRef
joinRefFromInt = joinRefFromInteger . toInteger

timerRefFromInt :: Int -> Either RefErr TimerRef
timerRefFromInt = timerRefFromInteger . toInteger

breakRefFromInt :: Int -> Either RefErr BreakRef
breakRefFromInt = breakRefFromInteger . toInteger

machineRefFromInteger :: Integer -> Either RefErr MachineRef
machineRefFromInteger = mkRefFromInteger MachineRef

regionRefFromInteger :: Integer -> Either RefErr RegionRef
regionRefFromInteger = mkRefFromInteger RegionRef

stateRefFromInteger :: Integer -> Either RefErr StateRef
stateRefFromInteger = mkRefFromInteger StateRef

caseRefFromInteger :: Integer -> Either RefErr CaseRef
caseRefFromInteger = mkRefFromInteger CaseRef

routeRefFromInteger :: Integer -> Either RefErr RouteRef
routeRefFromInteger = mkRefFromInteger RouteRef

joinRefFromInteger :: Integer -> Either RefErr JoinRef
joinRefFromInteger = mkRefFromInteger JoinRef

timerRefFromInteger :: Integer -> Either RefErr TimerRef
timerRefFromInteger = mkRefFromInteger TimerRef

breakRefFromInteger :: Integer -> Either RefErr BreakRef
breakRefFromInteger = mkRefFromInteger BreakRef

renderRefErr :: RefErr -> Text
renderRefErr err =
  case err of
    NegativeEr n ->
      "reference cannot be negative: " <> tshow n

    OverflowEr n ->
      "reference exceeds Word32 range: " <> tshow n

    FractionalEr sc ->
      "reference must be an integer: " <> tshow sc

    InvalidTextEr txt ->
      "invalid reference text: " <> txt

instance FromJSON MachineRef where
  parseJSON = withScientific "MachineRef" $ \sc ->
    case parseRefScientific MachineRef sc of
      Left err -> fail $ show err
      Right v  -> pure v

instance FromJSON RegionRef where
  parseJSON = withScientific "RegionRef" (either (fail . show) pure . parseRefScientific RegionRef)

instance FromJSON StateRef where
  parseJSON = withScientific "StateRef" (either (fail . show) pure . parseRefScientific StateRef)

instance FromJSON CaseRef where
  parseJSON = withScientific "CaseRef" (either (fail . show) pure . parseRefScientific CaseRef)

instance FromJSON RouteRef where
  parseJSON = withScientific "RouteRef" (either (fail . show) pure . parseRefScientific RouteRef)

instance FromJSON JoinRef where
  parseJSON = withScientific "JoinRef" (either (fail . show) pure . parseRefScientific JoinRef)

instance FromJSON TimerRef where
  parseJSON = withScientific "TimerRef" (either (fail . show) pure . parseRefScientific TimerRef)

instance FromJSON BreakRef where
  parseJSON = withScientific "BreakRef" (either (fail . show) pure . parseRefScientific BreakRef)

instance FromJSONKey MachineRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText MachineRef)

instance FromJSONKey RegionRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText RegionRef)

instance FromJSONKey StateRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText StateRef)

instance FromJSONKey CaseRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText CaseRef)

instance FromJSONKey RouteRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText RouteRef)

instance FromJSONKey JoinRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText JoinRef)

instance FromJSONKey TimerRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText TimerRef)

instance FromJSONKey BreakRef where
  fromJSONKey = FromJSONKeyTextParser (either (fail . show) pure . parseRefText BreakRef)


mkRefFromInteger :: (Word32 -> a) -> Integer -> Either RefErr a
mkRefFromInteger wrap n
  | n < 0 = Left (NegativeEr n)
  | n > toInteger (maxBound :: Word32) = Left (OverflowEr n)
  | otherwise = Right (wrap (fromInteger n))


parseRefScientific :: (Word32 -> a) -> Scientific -> Either String a
parseRefScientific wrap sc
  | not (Sci.isInteger sc) = Left (T.unpack (renderRefErr (FractionalEr sc)))
  | otherwise =
      case Sci.toBoundedInteger sc :: Maybe Word32 of
        Just w -> Right (wrap w)
        Nothing -> Left . show $ renderRefErr (InvalidTextEr (tshow sc))


parseRefText :: (Word32 -> a) -> Text -> Either String a
parseRefText wrap raw =
  let
    txt = T.strip raw
  in case TR.signed TR.decimal txt of
       Right (n, rest)
         | T.null rest ->
             either (Left . T.unpack . renderRefErr) Right (mkRefFromInteger wrap n)
       _ -> Left (T.unpack (renderRefErr (InvalidTextEr txt)))

tshow :: Show a => a -> Text
tshow = T.pack . show