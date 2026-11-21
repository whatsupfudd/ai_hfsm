{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
-- | W3C traceparent propagation and OTLP/HTTP JSON exporter. No tracing
-- code is called from planStep: replay remains pure and deterministic.
module Hfsm.Operations.Trace
  ( SpanContext(..), SpanRecord(..), SpanSink(..)
  , parseTraceparent, formatTraceparent, newRootContext, newChildContext
  , withSpan, otlpHttpSink, stdoutSink
  ) where

import Control.Exception (SomeException, try, throwIO)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=), encode)
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as U
import Network.HTTP.Client (Manager, Request(..), RequestBody(..), httpLbs, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (hContentType)

-- | A context is not a proof of causation; it is a propagated diagnostic key.
data SpanContext = SpanContext
  { traceId :: Text
  , spanId :: Text
  , flags :: Text
  }
  deriving (Eq, Show)

data SpanRecord = SpanRecord
  { context :: SpanContext
  , parentSpanId :: Maybe Text
  , name :: Text
  , startedAt :: UTCTime
  , endedAt :: UTCTime
  , attributes :: [(Text,Text)]
  , errorMessage :: Maybe Text
  }
  deriving (Eq, Show)

newtype SpanSink = SpanSink { emitSpan :: SpanRecord -> IO () }


parseTraceparent :: Text -> Maybe SpanContext
parseTraceparent raw = case T.splitOn "-" (T.toLower raw) of
  [version,tid,sid,flagsValue]
    | version=="00", isHex tid 32, isHex sid 16,
      isHex flagsValue 2, T.any (/='0') tid, T.any (/='0') sid ->
        Just SpanContext {traceId=tid,spanId=sid,flags=flagsValue}
  _ -> Nothing
 where
  isHex t n = T.length t==n && T.all isHexDigit t

formatTraceparent :: SpanContext -> Text
formatTraceparent ctx = "00-"<>ctx.traceId<>"-"<>ctx.spanId<>"-"<>ctx.flags

hexUuid :: IO Text
hexUuid = T.filter (/='-') . U.toText <$> nextRandom


newRootContext :: IO SpanContext
newRootContext = do
  tid <- hexUuid
  sid <- T.take 16 <$> hexUuid
  pure SpanContext { traceId=tid, spanId=sid, flags="01" }


newChildContext :: SpanContext -> IO SpanContext
newChildContext parent = do
  sid <- T.take 16 <$> hexUuid
  pure SpanContext { traceId=parent.traceId, spanId=sid, flags=parent.flags }


-- | Telemetry export failure must not turn a successfully committed machine
-- transition into an application failure or an accidental retry.
withSpan :: SpanSink -> Maybe SpanContext -> Text -> [(Text,Text)] -> IO a -> IO a
withSpan sink maybeParent operation attrs action = do
  current <- maybe newRootContext newChildContext maybeParent
  started <- getCurrentTime
  result <- try action
  ended <- getCurrentTime
  let problem = case result of
        Left (err :: SomeException) -> Just (T.pack (show err))
        Right _ -> Nothing
      record = SpanRecord
        { context=current, parentSpanId=fmap (.spanId) maybeParent
        , name=operation, startedAt=started, endedAt=ended
        , attributes=attrs, errorMessage=problem }
  _ <- try (sink.emitSpan record) :: IO (Either SomeException ())
  either throwIO pure result


stdoutSink :: SpanSink
stdoutSink = SpanSink (putStrLn . show)


-- | OTLP/HTTP JSON POST to a trusted Collector /v1/traces request URL.
-- Collector errors are ignored by withSpan, not by the sink itself.
otlpHttpSink :: Manager -> Request -> Text -> SpanSink
otlpHttpSink manager base service = SpanSink $ \record -> do
  let request = base
        { method="POST"
        , requestBody=RequestBodyLBS (encode (otlpPayload service record))
        , requestHeaders=(hContentType,"application/json"):requestHeaders base }
  response <- httpLbs request manager
  if statusCode (responseStatus response)>=300
    then fail ("OTLP exporter returned HTTP "<>show (statusCode (responseStatus response)))
    else pure ()


otlpPayload :: Text -> SpanRecord -> Value
otlpPayload service record = object
  [ "resourceSpans" .= [object
      [ "resource" .= object ["attributes" .= [attribute ("service.name"::Text) service ]]
      , "scopeSpans" .= [object
          [ "scope" .= object ["name" .= ("ai_hfsm.ct06"::Text)]
          , "spans" .= [object
              [ "traceId" .= record.context.traceId
              , "spanId" .= record.context.spanId
              , "parentSpanId" .= record.parentSpanId
              , "name" .= record.name
              , "kind" .= (1::Int)
              , "startTimeUnixNano" .= nanos record.startedAt
              , "endTimeUnixNano" .= nanos record.endedAt
              , "attributes" .= map (uncurry attribute) record.attributes
              , "status" .= object ["code" .= (if record.errorMessage==Nothing then (1::Int) else 2)
                                 ,"message" .= record.errorMessage]
              ]]]]]]]
  where
  attribute key value = object ["key" .= key,"value" .= object ["stringValue" .= value]]
  nanos :: UTCTime -> Text
  nanos = T.pack . show . (floor :: Rational -> Integer) . (*1000000000) . toRational . utcTimeToPOSIXSeconds
