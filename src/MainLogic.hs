module MainLogic
where

import Data.Text (pack)
import qualified System.Environment as Env

import qualified Options as Opt
import qualified Options.Cli as Opt (CliOptions (..), EnvOptions (..), Command (..))
import qualified Options.ConfFile as Opt (FileOptions (..))
import Commands as Cmd


runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO ()
runWithOptions cliOptions fileOptions = do
  -- putStrLn $ "@[runWithOptions] cliOpts: " <> show cliOptions
  -- putStrLn $ "@[runWithOptions] fileOpts: " <> show fileOptions
  case cliOptions.job of
    Nothing -> do
      putStrLn "@[runWithOptions] start on nil command."
    Just aJob -> do
      -- Get environmental context in case it's required in the merge. Done here to keep the merge pure:
      mbHome <- Env.lookupEnv "thfsmHOME"
      let
        envOptions = Opt.EnvOptions {
            Opt.appHome = mbHome
            -- TODO: put additional env vars.
          }
        -- switchboard to command executors:
        cmdExecutor =
          case aJob of
            Opt.HelpCmd -> Cmd.helpCmd
            Opt.VersionCmd -> Cmd.versionCmd
            -- HERE: add the other commands for this project.
      rtOptions <- Opt.mergeOptions cliOptions fileOptions envOptions
      result <- cmdExecutor rtOptions
      -- TODO: return a properly kind of conclusion.
      pure ()
