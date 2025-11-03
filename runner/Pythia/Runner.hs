{-# LANGUAGE QuasiQuotes #-}

-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Pythia.Runner
  ( -- * Runners
    runPythia,

    -- * Helpers
    getFinalConfig,
  )
where

import Effects.FileSystem.FileReader qualified as FR
import Effects.FileSystem.PathReader qualified as PR
import Effects.Optparse (MonadOptparse)
import Effects.Optparse qualified as OApp
import Pythia.Prelude
import Pythia.Runner.Args (Args, WithDisabled (Disabled, With), parserInfo)
import Pythia.Runner.Command
  ( PythiaCommand2,
    PythiaCommandP
      ( BatteryCmd,
        GlobalIpCmd,
        MemoryCmd,
        NetConnCmd,
        NetInterfaceCmd,
        TimeCmd
      ),
  )
import Pythia.Runner.Command.Battery qualified as Battery
import Pythia.Runner.Command.GlobalIp qualified as GlobalIp
import Pythia.Runner.Command.Memory qualified as Memory
import Pythia.Runner.Command.NetConn qualified as NetConn
import Pythia.Runner.Command.NetInterface qualified as NetInterface
import Pythia.Runner.Command.Time qualified as Time
import Pythia.Runner.Toml (Toml)
import Pythia.Runner.Toml qualified as PToml
import TOML qualified

-- | Reads cli args and prints the results to stdout.
--
-- @since 0.1
runPythia ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadOptparse m,
    MonadTerminal m,
    MonadTime m,
    MonadTypedProcess m
  ) =>
  m ()
runPythia =
  getFinalConfig >>= \case
    BatteryCmd app field -> Battery.handleBattery app field
    GlobalIpCmd app cfg -> GlobalIp.handleGlobalIp app cfg
    MemoryCmd app field format -> Memory.handleMemory app field format
    NetInterfaceCmd app device field -> NetInterface.handleNetInterface app device field
    NetConnCmd app field -> NetConn.handleNetConn app field
    TimeCmd dest format -> Time.handleTime format dest

-- | Retrieves the final configured command based on the CLI args and
-- possible TOML file.
--
-- @since 0.1
getFinalConfig ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadOptparse m
  ) =>
  m PythiaCommand2
getFinalConfig = do
  -- args has phase 1 command
  args <- OApp.execParser parserInfo
  mTomlConfig <- getTomlConfig args

  -- evolve phase 1 command to phase 2 command, using toml config if it exists
  let cliCmdP1 = args ^. #command
  throwLeft (PToml.combineConfigs cliCmdP1 mTomlConfig)

getTomlConfig ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m
  ) =>
  Args ->
  m (Maybe Toml)
getTomlConfig args = do
  tomlPath <-
    case args ^. #config of
      -- Case 1: User explicitly wants toml config ignored
      Just Disabled -> pure Nothing
      -- Case 2: User supplies a toml path -> use it
      Just (With userPath) -> pure $ Just userPath
      Nothing -> do
        xdgPath <- (</> [osp|config.toml|]) <$> PR.getXdgConfig [osp|pythia|]
        xdgConfigExists <- PR.doesFileExist xdgPath
        if xdgConfigExists
          then -- Case 3: Toml exists at expected XDG -> use it
            pure $ Just xdgPath
          else -- Case 4: No Toml -> do nothing
            pure Nothing

  -- decode toml if we have a path
  maybe (pure Nothing) decodeTomlM tomlPath
  where
    decodeTomlM p =
      FR.readFileUtf8ThrowM p
        >>= ( \case
                Left err -> throwM err
                Right c -> pure c
            )
        . TOML.decode
