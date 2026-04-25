{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Validate.Error
  ( Severity(..)
  , ValidateLoc(..)
  , ValidateErr(..)
  , mkValidateErr
  , infoErr
  , warnErr
  , errorErr
  , atMachine
  , atRegion
  , atState
  , atCase
  , atRoute
  , atJoin
  , withLoc
  , withMsg
  , withCode
  , isInfo
  , isWarn
  , isError
  ) where

import Control.DeepSeq (NFData)

import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Hfsm.Core.Ref (CaseRef, JoinRef, RegionRef, RouteRef, StateRef)


data Severity =
    InfoSv
  | WarnSv
  | ErrorSv
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data ValidateLoc =
    MachineVl
  | RegionVl RegionRef
  | StateVl StateRef
  | CaseVl CaseRef
  | RouteVl RouteRef
  | JoinVl JoinRef
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data ValidateErr = ValidateErr
  { code :: Text
  , msg :: Text
  , sev :: Severity
  , loc :: Maybe ValidateLoc
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

mkValidateErr :: Text -> Text -> Severity -> Maybe ValidateLoc -> ValidateErr
mkValidateErr code msg sev loc =
  ValidateErr
    { code = code
    , msg = msg
    , sev = sev
    , loc = loc
    }

infoErr :: Text -> Text -> Maybe ValidateLoc -> ValidateErr
infoErr code msg loc = mkValidateErr code msg InfoSv loc

warnErr :: Text -> Text -> Maybe ValidateLoc -> ValidateErr
warnErr code msg loc = mkValidateErr code msg WarnSv loc

errorErr :: Text -> Text -> Maybe ValidateLoc -> ValidateErr
errorErr code msg loc = mkValidateErr code msg ErrorSv loc

atMachine :: ValidateErr -> ValidateErr
atMachine err = err { loc = Just MachineVl }

atRegion :: RegionRef -> ValidateErr -> ValidateErr
atRegion ref err = err { loc = Just (RegionVl ref) }

atState :: StateRef -> ValidateErr -> ValidateErr
atState ref err = err { loc = Just (StateVl ref) }

atCase :: CaseRef -> ValidateErr -> ValidateErr
atCase ref err = err { loc = Just (CaseVl ref) }

atRoute :: RouteRef -> ValidateErr -> ValidateErr
atRoute ref err = err { loc = Just (RouteVl ref) }

atJoin :: JoinRef -> ValidateErr -> ValidateErr
atJoin ref err = err { loc = Just (JoinVl ref) }

withLoc :: Maybe ValidateLoc -> ValidateErr -> ValidateErr
withLoc loc err = err { loc = loc }

withMsg :: Text -> ValidateErr -> ValidateErr
withMsg msg err = err { msg = msg }

withCode :: Text -> ValidateErr -> ValidateErr
withCode code err = err { code = code }

isInfo :: ValidateErr -> Bool
isInfo err =
  case err.sev of
    InfoSv -> True
    WarnSv -> False
    ErrorSv -> False

isWarn :: ValidateErr -> Bool
isWarn err =
  case err.sev of
    InfoSv -> False
    WarnSv -> True
    ErrorSv -> False

isError :: ValidateErr -> Bool
isError err =
  case err.sev of
    InfoSv -> False
    WarnSv -> False
    ErrorSv -> True