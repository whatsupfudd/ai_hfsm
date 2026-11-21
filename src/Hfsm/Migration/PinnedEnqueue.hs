{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Safe enqueue into an explicitly migrated instance. The producer names
-- its exact intended release; the version check and enqueue share one row
-- lock and one transaction. The SQL trigger rejects the legacy unpinned
-- enqueue path for migrated instances.
module Hfsm.Migration.PinnedEnqueue (enqueuePinned) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TS
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement)
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Core.Digest (specDigestText)
import Hfsm.Runtime.Registry (MachineKey(..))
import Hfsm.Store.Class (StoreErr(..))
import Hfsm.Store.Postgres.Types (Runner(..), EnqueueRq(..))
import qualified Hfsm.Store.Postgres.Statements as S
import qualified Hfsm.Store.Postgres.Transaction as X

setRelease :: Statement Text Text
setRelease = [singletonStatement|
 SELECT set_config('hfsm.enqueue_release', $1 :: text, true) :: text
|]

enqueuePinned :: Runner -> MachineKey -> EnqueueRq
              -> IO (Either StoreErr UUID)
enqueuePinned runner expected rq = do
  result <- runSession runner $
    TS.transactionNoRetry TS.ReadCommitted TS.Write $ do
      row <- Tx.statement rq.instanceId S.lockInstance
      case row of
        Nothing -> pure (Left (NotFoundEr "enqueuePinned: instance missing"))
        Just (_,_,_,_,_,name,version,digest,_,_,_)
          | name /= machineNameText expected.name ||
            version /= machineVersionText expected.version ||
            digest /= specDigestText expected.digest ->
              pure (Left (ConflictEr "enqueuePinned: producer release does not match instance"))
          | otherwise -> do
              _ <- Tx.statement
                (machineNameText expected.name <> "@" <>
                 machineVersionText expected.version <> "#" <>
                 specDigestText expected.digest) setRelease
              X.enqueueTx rq
  pure $ case result of
    Left err -> Left (BackendEr (T.pack (show err)))
    Right value -> value
