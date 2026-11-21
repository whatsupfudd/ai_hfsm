{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Strict release registration: one immutable graph and executable build
-- identity per (machine name, version). Use this instead of legacy
-- registerMachine for all CT-05 releases (including migration targets).
module Hfsm.History.Release
  ( ReleaseErr(..), registerPinnedRelease, verifyPinnedRelease ) where

import Data.Int ()
import Data.Text (Text)
import qualified Data.Text as T
import Hasql.Statement (Statement)
import Hasql.TH (resultlessStatement, maybeStatement)
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TS
import qualified Hasql.Session as Hs
import Hfsm.Compile (CompiledMachine(..))
import Hfsm.Core.Digest (specDigestText, graphDigestText)
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Graph.Def (MachineGraph(..))
import Hfsm.Runtime.Registry (machineKeyOf, MachineKey(..))
import Hfsm.Store.Postgres.Types (Runner(..))

data ReleaseErr
  = ReleaseBackend Text
  | ReleaseIdentityConflict Text
  | ReleaseLegacyUnpinned Text
  deriving (Eq, Show)

insertPinned :: Statement (Text,Text,Text,Text,Text) ()
insertPinned = [resultlessStatement|
 INSERT INTO hfsm.machine_release
   (machine_name,machine_version,spec_digest,graph_digest,implementation_id)
 VALUES ($1 :: text,$2 :: text,$3 :: text,$4 :: text,$5 :: text)
 ON CONFLICT (machine_name,machine_version) DO NOTHING
|]

readPinned :: Statement (Text,Text) (Maybe (Text,Maybe Text,Maybe Text))
readPinned = [maybeStatement|
 SELECT spec_digest :: text,graph_digest :: text?,implementation_id :: text?
 FROM hfsm.machine_release WHERE machine_name=$1 :: text AND machine_version=$2 :: text
|]

-- | Caller passes an immutable CI artifact identifier covering compiled
-- reaction functions and codecs, NOT merely the serializable graph digest.
registerPinnedRelease
  :: Runner -> CompiledMachine st sg hf ctx cmd child -> Text
  -> IO (Either ReleaseErr ())
registerPinnedRelease runner compiled implementationId
  | T.length implementationId < 12 =
      pure (Left (ReleaseIdentityConflict "implementationId must be >= 12 characters"))
  | otherwise = do
      let key = machineKeyOf compiled
          name = machineNameText key.name
          version = machineVersionText key.version
          digest = specDigestText key.digest
          graphHash = graphDigestText compiled.graph.digest
      executed <- runSession runner $ TS.transactionNoRetry TS.ReadCommitted TS.Write $ do
        Tx.statement (name,version,digest,graphHash,implementationId) insertPinned
        Tx.statement (name,version) readPinned
      pure $ case executed of
        Left err -> Left (ReleaseBackend (T.pack (show err)))
        Right Nothing -> Left (ReleaseIdentityConflict "registered row vanished")
        Right (Just (storedDigest,storedGraph,storedImpl))
          | storedDigest /= digest -> Left (ReleaseIdentityConflict "version already registered with a different spec digest")
          | storedGraph == Nothing || storedImpl == Nothing ->
              Left (ReleaseLegacyUnpinned "legacy registration has no implementation provenance; do not silently update immutable release")
          | storedGraph /= Just graphHash -> Left (ReleaseIdentityConflict "graph digest mismatch")
          | storedImpl /= Just implementationId -> Left (ReleaseIdentityConflict "executable fingerprint mismatch")
          | otherwise -> Right ()

verifyPinnedRelease :: Runner -> CompiledMachine st sg hf ctx cmd child
                    -> Text -> IO (Either ReleaseErr ())
verifyPinnedRelease runner compiled implementationId = do
  let key = machineKeyOf compiled
  result <- runSession runner $ Hs.statement
    (machineNameText key.name,machineVersionText key.version) readPinned
  pure $ case result of
    Left err -> Left (ReleaseBackend (T.pack (show err)))
    Right Nothing -> Left (ReleaseIdentityConflict "machine release not registered")
    Right (Just (digest,graphHash,impl))
      | digest /= specDigestText key.digest ||
        graphHash /= Just (graphDigestText compiled.graph.digest) ||
        impl /= Just implementationId -> Left (ReleaseIdentityConflict "pinned release mismatch")
      | otherwise -> Right ()
