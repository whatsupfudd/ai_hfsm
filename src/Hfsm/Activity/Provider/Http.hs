-- | Optional concrete HTTP/JSON provider. The endpoint is supplied by
-- trusted application configuration, never by arbitrary outbox content.
-- Use a TLS-capable Manager for HTTPS (http-client-tls).
--
-- IMPORTANT: Setting safeReplay=True is correct ONLY if the receiving API
-- reliably deduplicates requests by Idempotency-Key across worker crashes.
module Hfsm.Activity.Provider.Http (newHttpProvider, httpProviderWithRequest) where

import Data.Aeson (Value(..), encode, eitherDecode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)

import Network.HTTP.Client
  ( Manager, Request(..), RequestBody(..), httpLbs, parseRequest
  , responseBody, responseStatus )
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)

import Hfsm.Activity.Types (Provider(..), ProviderFailure(..), ActivityLease(..))

-- | POST every effect using the stable key, regardless of retry count.
-- A body of the form {"kind":"cmd","payload":...} is forwarded unchanged.
newHttpProvider :: Manager -> Text -> String -> Bool -> IO Provider
newHttpProvider manager effectKind fixedUrl safeReplay = do
  base <- parseRequest fixedUrl
  pure (httpProviderWithRequest manager effectKind base safeReplay)

-- | Preconfigured Request variant for application-owned Authorization headers,
-- TLS settings and other trusted provider configuration. Do not store tokens
-- in HFSM outbox payloads.
httpProviderWithRequest :: Manager -> Text -> Request -> Bool -> Provider
httpProviderWithRequest manager effectKind base safeReplay = Provider
    { kind = effectKind
    , supportsIdempotency = safeReplay
    , deliver = \effect -> do
        let request = base
              { method = "POST"
              , requestBody = RequestBodyLBS (encode effect.payload)
              , requestHeaders =
                  [ (hContentType, "application/json")
                  , ("Idempotency-Key", TE.encodeUtf8 effect.effectKey)
                  ] <> requestHeaders base
              }
        response <- httpLbs request manager
        let httpCode = statusCode (responseStatus response)
            truncated = BL.take 65536 (responseBody response)
            bodyValue = case eitherDecode truncated of
              Right value -> value :: Value
              Left _ -> String (TE.decodeUtf8With lenientDecode (BL.toStrict truncated))
        if httpCode >= 200 && httpCode < 300
          then pure (Right (object
            [ "statusCode" .= httpCode, "body" .= bodyValue ]))
          else pure (Left ProviderFailure
            { message = "HTTP " <> T.pack (show httpCode) <> ": " <> T.take 1024 (T.pack (show bodyValue))
            , retryable = httpCode == 408 || httpCode == 429 || httpCode >= 500
            , ambiguous = httpCode == 408 || httpCode >= 500
            })
    }
