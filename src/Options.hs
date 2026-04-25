module Options  (
  module Cl
  , module Fo
  , module Rt
  , mergeOptions
 )
where

import Control.Monad.State ( MonadState (put), MonadIO, runStateT, State, StateT, modify, lift, liftIO )
import Control.Monad.Except ( ExceptT, MonadError (throwError) )
import Data.Functor.Identity ( Identity (..) )

import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified System.IO.Error as Serr
import qualified Control.Exception as Cexc
import qualified System.Posix.Env as Senv
import qualified System.Directory as Sdir


import qualified Options.Cli as Cl (CliOptions (..), EnvOptions (..))
import qualified Options.ConfFile as Fo (FileOptions (..))
import qualified Options.Runtime as Rt (RunOptions (..), defaultRun)


type ConfError = Either String ()
type RunOptSt = State Rt.RunOptions ConfError
type RunOptIOSt = StateT Rt.RunOptions IO ConfError


mconf :: MonadState s m => Maybe t -> (t -> s -> s) -> m ()
mconf mbOpt setter =
  case mbOpt of
    Nothing -> pure ()
    Just opt -> modify $ setter opt

innerConf :: MonadState s f => (t1 -> s -> s) -> (t2 -> StateT t1 f (Either a b)) -> t1 -> Maybe t2 -> f ()
innerConf updState innerParser defaultVal mbOpt =
  case mbOpt of
    Nothing -> pure ()
    Just anOpt -> do
      (result, updConf) <- runStateT (innerParser anOpt) defaultVal
      case result of
        Left errMsg -> pure ()
        Right _ -> modify $ updState updConf


mergeOptions :: Cl.CliOptions -> Fo.FileOptions -> Cl.EnvOptions -> IO Rt.RunOptions
mergeOptions cli file env = do
  (result, runtimeOpts) <- runStateT (parseOptions cli file) (Rt.defaultRun )
  case result of
    Left errMsg -> error errMsg
    Right _ -> pure runtimeOpts
  where
  parseOptions :: Cl.CliOptions -> Fo.FileOptions -> RunOptIOSt
  parseOptions cli file = do
    mconf cli.debug $ \nVal s -> s { Rt.debug = nVal }
    pure $ Right ()




-- | resolveEnvValue resolves an environment variable value.
resolveEnvValue :: FilePath -> IO (Maybe FilePath)
resolveEnvValue aVal =
  case head aVal of
      '$' ->
        let
          (envName, leftOver) = break ('/' ==) aVal
        in do
        mbEnvValue <- Senv.getEnv $ tail envName
        case mbEnvValue of
          Nothing -> pure Nothing
          Just aVal -> pure . Just $ aVal <> leftOver
      _ -> pure $ Just aVal

