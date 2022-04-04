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
    RunApp (..),

    -- ** Errors
    NetInterfaceException (..),
    uncheckNetInterface,
    rethrowNetInterface,

    -- *** Sub-errors
    IpException (..),
    NmCliException (..),
  )
where

import Pythia.Data.RunApp (RunApp (..))
import Pythia.Prelude
import Pythia.Services.NetInterface.Ip (IpException)
import Pythia.Services.NetInterface.Ip qualified as Ip
import Pythia.Services.NetInterface.NmCli (NmCliException)
import Pythia.Services.NetInterface.NmCli qualified as NmCli
import Pythia.Services.NetInterface.Types
  ( NetInterface (..),
    NetInterfaceApp (..),
    NetInterfaceConfig (..),
    NetInterfaceException (..),
    NetInterfaceState (..),
    NetInterfaceType (..),
    NetInterfaces (..),
  )
import Pythia.Services.Types (Device (..), Ipv4Address (..), Ipv6Address (..))
import Pythia.ShellApp (AppAction (..), MultiExceptions (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for interface information by detecting supported
-- apps. Tries, in the following order: ['NetInterfaceNmCli',
-- 'NetInterfaceIp'].
--
-- @since 0.1.0.0
queryNetInterfaces ::
  ( MonadCatch m,
    MonadIO m,
    Throws NetInterfaceException
  ) =>
  m NetInterfaces
queryNetInterfaces = queryNetInterfacesConfig mempty

-- | Queries for network information based on the configuration.
--
-- @since 0.1.0.0
queryNetInterfacesConfig ::
  ( MonadCatch m,
    MonadIO m,
    Throws NetInterfaceException
  ) =>
  NetInterfaceConfig ->
  m NetInterfaces
queryNetInterfacesConfig config = do
  case config ^. #interfaceApp of
    Many -> rethrowNetInterface @MultiExceptions $ ShellApp.tryAppActions allApps
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
  ( MonadCatch m,
    MonadIO m,
    Throws NetInterfaceException
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
  ( MonadCatch m,
    MonadIO m,
    Throws NetInterfaceException
  ) =>
  NetInterfaceApp ->
  m NetInterfaces
toSingleShellApp NetInterfaceNmCli = rethrowNetInterface @NmCliException NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceIp = rethrowNetInterface @IpException Ip.netInterfaceShellApp

-- | 'uncheck' specialized to 'NetInterfaceException'.
--
-- @since 0.1.0.0
uncheckNetInterface :: ((Throws NetInterfaceException) => m a) -> m a
uncheckNetInterface = uncheck (Proxy @NetInterfaceException)

-- | Rethrows a checked exception as a 'NetInterfaceException'.
--
-- @since 0.1.0.0
rethrowNetInterface :: forall e m a. (Exception e, MonadCatch m, Throws NetInterfaceException) => (Throws e => m a) -> m a
rethrowNetInterface = handle (\(ex :: e) -> throw $ MkNetInterfaceErr ex)
