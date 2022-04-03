-- | This module exports interface related services.
--
-- @since 0.1.0.0
module Pythia.Services.NetInterface
  ( -- * Queries
    queryNetInterfaces,
    queryNetInterfacesConfig,

    -- * Types
    NetInterfaces (..),
    NetInterface (..),
    NetInterfaceState (..),
    NetInterfaceType (..),
    Device (..),
    Ipv4Address (..),
    Ipv6Address (..),

    -- ** Configuration
    NetInterfaceConfig (..),
    NetInterfaceApp (..),

    -- ** Errors
    uncheckNetInterface,
    rethrowNetInterface,
    IpError (..),
    NmCliError (..),
  )
where

import Pythia.Data.RunApp (RunApp (..))
import Pythia.Prelude
import Pythia.Services.NetInterface.Ip (IpError)
import Pythia.Services.NetInterface.Ip qualified as Ip
import Pythia.Services.NetInterface.NmCli (NmCliError)
import Pythia.Services.NetInterface.NmCli qualified as NmCli
import Pythia.Services.NetInterface.Types
  ( NetInterface (..),
    NetInterfaceApp (..),
    NetInterfaceConfig (..),
    NetInterfaceState (..),
    NetInterfaceType (..),
    NetInterfaces (..),
  )
import Pythia.Services.Types (Device (..), Ipv4Address (..), Ipv6Address (..))
import Pythia.ShellApp (AppAction (..), CmdError (..), Exceptions (..), NoActionsRunError)
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U

-- | Attempts to query for interface information by detecting supported
-- apps. Tries, in the following order: ['NetInterfaceNmCli',
-- 'NetInterfaceIp'].
--
-- @since 0.1.0.0
queryNetInterfaces ::
  ( MonadCatch m,
    MonadIO m,
    Throws CmdError,
    Throws Exceptions,
    Throws IpError,
    Throws NmCliError,
    Throws NoActionsRunError
  ) =>
  m NetInterfaces
queryNetInterfaces = queryNetInterfacesConfig mempty

-- | Queries for network information based on the configuration.
--
-- @since 0.1.0.0
queryNetInterfacesConfig ::
  ( MonadCatch m,
    MonadIO m,
    Throws CmdError,
    Throws Exceptions,
    Throws IpError,
    Throws NmCliError,
    Throws NoActionsRunError
  ) =>
  NetInterfaceConfig ->
  m NetInterfaces
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
  ( MonadIO m,
    Throws CmdError,
    Throws IpError,
    Throws NmCliError
  ) =>
  Maybe Device ->
  NetInterfaceApp ->
  m NetInterfaces
queryNetInterfacesDeviceApp Nothing app = toSingleShellApp app
queryNetInterfacesDeviceApp (Just device) app =
  filterDevice device <$> toSingleShellApp app

filterDevice :: Device -> NetInterfaces -> NetInterfaces
filterDevice device (MkNetInterfaces ifs) =
  MkNetInterfaces $
    filter ((== device) . view #idevice) ifs

toSingleShellApp ::
  ( MonadIO m,
    Throws CmdError,
    Throws IpError,
    Throws NmCliError
  ) =>
  NetInterfaceApp ->
  m NetInterfaces
toSingleShellApp NetInterfaceNmCli = NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceIp = Ip.netInterfaceShellApp

-- | Unchecks all exceptions returns by network interface queries.
--
-- @since 0.1.0.0
uncheckNetInterface ::
  ( ( Throws CmdError,
      Throws IpError,
      Throws Exceptions,
      Throws NmCliError,
      Throws NoActionsRunError
    ) =>
    IO a
  ) ->
  IO a
uncheckNetInterface = U.uncheck5 @CmdError @IpError @Exceptions @NmCliError @NoActionsRunError

-- | Lifts all network interface errors into the current monad, given a suitably
-- polymorphic function.
--
-- @since 0.1.0.0
rethrowNetInterface ::
  MonadCatch m =>
  (forall e b. Either e b -> m b) ->
  ( ( Throws CmdError,
      Throws IpError,
      Throws Exceptions,
      Throws NmCliError,
      Throws NoActionsRunError
    ) =>
    m a
  ) ->
  m a
rethrowNetInterface = U.rethrow5 @CmdError @IpError @Exceptions @NmCliError @NoActionsRunError
