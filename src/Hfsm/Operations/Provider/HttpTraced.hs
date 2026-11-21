{-# LANGUAGE NoFieldSelectors #-}
-- | A CT-06 HTTP provider: exact CT-04 idempotency key plus W3C traceparent
-- from the durable source signal (carried by the outbox intent).
module Hfsm.Operations.Provider.HttpTraced (tracedHttpProvider) where

import Data.Aeson (Value(..), encode, eitherDecode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Hasql.Session as Session
import Network.HTTP.Client
  (Manager, Request(..), RequestBody(..), httpLbs, responseBody, responseStatus)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)
import Hfsm.Activity.Types (Provider(..), ProviderFailure(..), ActivityLease(..))
import Hfsm.Operations.Trace (formatTraceparent, newRootContext, parseTraceparent)
import qualified Hfsm.Operations.Postgres.Statements as S
import Hfsm.Store.Postgres.Types (Runner(..))

-- The URL, auth headers and TLS manager must be supplied by trusted config.
tracedHttpProvider :: Runner -> Manager -> Text -> Request -> Bool -> Provider
tracedHttpProvider runner manager effectKind base safeReplay = Provider
  { kind=effectKind
  , supportsIdempotency=safeReplay
  , deliver = \effect -> do
      traceResult <- runSession runner (Session.statement effect.effectId S.readEffectTraceparent)
      case traceResult of
        Left problem -> pure (Left ProviderFailure
          {message=T.pack (show problem),retryable=True,ambiguous=False})
        Right savedTrace -> do
          ctx <- case savedTrace >>= parseTraceparent of
            Just prior -> pure prior
            Nothing -> newRootContext
          let request=base
                { method="POST"
                , requestBody=RequestBodyLBS (encode effect.payload)
                , requestHeaders=
                    [ (hContentType,"application/json")
                    , ("Idempotency-Key",TE.encodeUtf8 effect.effectKey)
                    , ("traceparent",TE.encodeUtf8 (formatTraceparent ctx))
                    ] <> requestHeaders base }
          response <- httpLbs request manager
          let code=statusCode (responseStatus response)
              body=BL.take 65536 (responseBody response)
              value=case eitherDecode body of
                Right v -> v :: Value
                Left _ -> String (TE.decodeUtf8With lenientDecode (BL.toStrict body))
          if code>=200 && code<300
            then pure (Right (object ["statusCode" .= code,"body" .= value]))
            else pure (Left ProviderFailure
              { message="HTTP "<>T.pack (show code)<>" "<>T.take 1024 (T.pack (show value))
              , retryable=code==408 || code==429 || code>=500
              , ambiguous=code==408 || code>=500 })
  }
