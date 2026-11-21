-- | Read-only, page-based access to the durable audit trail.
module Hfsm.History.Postgres
  ( loadHistoryPage, loadSnapshotRevision, foldHistory ) where

import Data.Aeson (Result(..), FromJSON, Value, fromJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Hasql.Session as Session
import Hfsm.History.Types (HistoryErr(..), HistoryTurn(..))
import Hfsm.Runtime.Model (SnapshotRw)
import Hfsm.Store.Postgres.Types (Runner(..))
import qualified Hfsm.History.Statements as S

parseValue :: FromJSON a => Value -> Either HistoryErr a
parseValue v = case fromJSON v of
  Error err -> Left (HistoryDecode (T.pack err))
  Success x -> Right x

loadHistoryPage :: Runner -> UUID -> Int64 -> Int
                -> IO (Either HistoryErr [HistoryTurn])
loadHistoryPage runner instanceId fromRevision pageSize = do
  result <- runSession runner $ Session.statement
    (instanceId, max 1 fromRevision, fromIntegral (max 1 (min 1000 pageSize))) S.loadTurns
  pure $ case result of
    Left err -> Left (HistoryBackend (T.pack (show err)))
    Right rows -> traverse parseValue (V.toList rows)

loadSnapshotRevision :: Runner -> UUID -> Int64
                     -> IO (Either HistoryErr (Maybe SnapshotRw))
loadSnapshotRevision runner instanceId revision = do
  result <- runSession runner $ Session.statement (instanceId,revision) S.snapshotAt
  pure $ case result of
    Left err -> Left (HistoryBackend (T.pack (show err)))
    Right value -> traverse parseValue value

-- | Page through a stable historical prefix up to the caller's maxRevision.
-- Capture the max revision before starting if concurrent commits are possible.
foldHistory :: Runner -> UUID -> Int64 -> Int
            -> ([HistoryTurn] -> IO ()) -> IO (Either HistoryErr ())
foldHistory runner instanceId maxRevision pageSize consume = go 1
 where
  go fromRev
    | fromRev > maxRevision = pure (Right ())
    | otherwise = do
        loaded <- loadHistoryPage runner instanceId fromRev pageSize
        case loaded of
          Left err -> pure (Left err)
          Right [] -> pure (Left (HistoryDecode "missing history revision"))
          Right allRows -> do
            let rows = takeWhile (\r -> r.revision <= maxRevision) allRows
            case rows of
              [] -> pure (Right ())
              _  -> do
                consume rows
                let nextRev = (last rows).revision + 1
                if nextRev <= fromRev
                  then pure (Left (HistoryDecode "history page did not advance"))
                  else go nextRev
