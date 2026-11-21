{-# LANGUAGE NoFieldSelectors #-}
-- | Revision-ordered, idempotent, asynchronously rebuildable projections.
-- | A projection checkpoint is locked in the SAME transaction as view writes.
module Hfsm.Operations.Projection
  ( runProjectionTick, runProjectionWorker, rebuildProjection ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forever)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Hasql.Session as Session

import Hfsm.Operations.Types (OpErr(..))
import Hfsm.Store.Postgres.Types (Runner(..))
import qualified Hfsm.Operations.Postgres as Pg
import qualified Hfsm.Operations.Postgres.Statements as S

runProjectionTick :: Runner -> Text -> Int -> Int -> IO (Either OpErr Int)
runProjectionTick runner consumer instancesPerTick revsPerInstance = do
  found <- runSession runner $ Session.statement
    (consumer,fromIntegral (max 1 (min 1000 instancesPerTick))) S.projectCandidates
  case found of
    Left err -> pure (Left (OpBackend (fromShow err)))
    Right ids -> do
      results <- forM (V.toList ids) $ \instanceId ->
        Pg.projectInstance runner consumer instanceId revsPerInstance
      pure (sum <$> sequence results)

runProjectionWorker :: Runner -> Text -> Int -> Int -> Int -> IO ()
runProjectionWorker runner consumer instancesPerTick revsPerInstance pollMs = forever $ do
  result <- runProjectionTick runner consumer instancesPerTick revsPerInstance
  case result of
    Left err -> fail ("CT06 projection: " <> show err)
    Right _ -> pure ()
  threadDelay (max 20000 (pollMs * 1000))

-- | Administrative action: clears derived tables only, then replays every
-- immutable historical revision in bounded batches. Mutating history is forbidden.
rebuildProjection :: Runner -> Text -> UUID -> IO (Either OpErr Int)
rebuildProjection runner consumer instanceId = do
  cleared <- Pg.resetProjection runner consumer instanceId
  case cleared of
    Left err -> pure (Left err)
    Right () -> loop 0
 where
  loop count = do
    page <- Pg.projectInstance runner consumer instanceId 500
    case page of
      Left err -> pure (Left err)
      Right 0 -> pure (Right count)
      Right n -> loop (count+n)

fromShow :: Show a => a -> Text
fromShow = T.pack . show
