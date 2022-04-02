-- | This module exports interface related services.
--
-- @since 0.1.0.0
module Pythia.Services.Network.Interface
  ( -- * Queries
    queryNetInterfaces,
    queryNetInterfacesConfig,

    -- * Types
    Interfaces (..),
    Interface (..),
    InterfaceState (..),
    InterfaceType (..),
    Device (..),
    -- Ipv4Address (..),
    -- Ipv6Address (..),

    -- ** Configuration
    NetInterfaceConfig (..),
    NetInterfaceApp (..),

    -- ** Errors
    uncheckNetInterface,
    IpError (..),
    NmCliError (..),
  )
where

import Pythia.Data (RunApp (..))
import Pythia.Prelude
import Pythia.Services.Network.Interface.Ip (IpError)
import Pythia.Services.Network.Interface.Ip qualified as Ip
import Pythia.Services.Network.Interface.NmCli (NmCliError)
import Pythia.Services.Network.Interface.NmCli qualified as NmCli
import Pythia.Services.Network.Interface.Types
  ( Interface (..),
    InterfaceState (..),
    InterfaceType (..),
    Interfaces (..),
    NetInterfaceApp (..),
    NetInterfaceConfig (..),
  )
import Pythia.Services.Network.Types (Device (..))
import Pythia.ShellApp (AppAction (..), CmdError (..), Exceptions (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for interface information by detecting supported
-- apps. Tries, in the following order: ['NetInterfaceNmCli',
-- 'NetInterfaceIp'].
--
-- @since 0.1.0.0
queryNetInterfaces ::
  ( Throws CmdError,
    Throws Exceptions,
    Throws IpError,
    Throws NmCliError
  ) =>
  IO Interfaces
queryNetInterfaces = queryNetInterfacesConfig mempty

-- | Queries for network information based on the configuration.
--
-- @since 0.1.0.0
queryNetInterfacesConfig ::
  ( Throws CmdError,
    Throws Exceptions,
    Throws IpError,
    Throws NmCliError
  ) =>
  NetInterfaceConfig ->
  IO Interfaces
queryNetInterfacesConfig config = do
  case config ^. #interfaceApp of
    Many -> ShellApp.tryAppActions allApps
    Single app -> singleRun app
  where
    allApps =
      [ MkAppAction (singleRun NetInterfaceNmCli) NmCli.supported (show NetInterfaceNmCli),
        MkAppAction (singleRun NetInterfaceIp) Ip.supported (show NetInterfaceIp)
      ]
    singleRun a =
      queryNetInterfacesDeviceApp device a
    device = config ^. #interfaceDevice

queryNetInterfacesDeviceApp ::
  ( Throws CmdError,
    Throws IpError,
    Throws NmCliError
  ) =>
  Maybe Device ->
  NetInterfaceApp ->
  IO Interfaces
queryNetInterfacesDeviceApp Nothing app = toSingleShellApp app
queryNetInterfacesDeviceApp (Just device) app =
  filterDevice device <$> toSingleShellApp app

filterDevice :: Device -> Interfaces -> Interfaces
filterDevice device (MkInterfaces ifs) =
  MkInterfaces $
    filter ((== device) . view #idevice) ifs

toSingleShellApp ::
  ( Throws CmdError,
    Throws IpError,
    Throws NmCliError
  ) =>
  NetInterfaceApp ->
  IO Interfaces
toSingleShellApp NetInterfaceNmCli = NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceIp = Ip.netInterfaceShellApp

-- | Unchecks all exceptions returns by network interface queries.
--
-- @since 0.1.0.0
uncheckNetInterface ::
  ( ( Throws CmdError,
      Throws IpError,
      Throws Exceptions,
      Throws NmCliError
    ) =>
    IO a
  ) ->
  IO a
uncheckNetInterface = uncheck4 @CmdError @IpError @Exceptions @NmCliError
