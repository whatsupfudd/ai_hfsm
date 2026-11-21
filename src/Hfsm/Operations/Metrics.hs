-- | Lightweight durable tenant counters suitable for scraping or dashboard
-- materialization. Metrics never mutate authoritative or projection rows.
module Hfsm.Operations.Metrics (tenantMetrics) where

import Data.Aeson (Value)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Hasql.Session as Session
import Hfsm.Operations.Types (OpErr(..))
import Hfsm.Store.Postgres.Types (Runner(..))
import qualified Hfsm.Operations.Postgres.Statements as S

tenantMetrics :: Runner -> UUID -> IO (Either OpErr Value)
tenantMetrics runner tenant = do
  result <- runSession runner $ Session.statement tenant S.tenantMetrics
  pure $ either (Left . OpBackend . T.pack . show) Right result
