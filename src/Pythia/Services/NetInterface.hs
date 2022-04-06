-- | This module exports interface related services.
--
-- @since 0.1.0.0
module Pythia.Services.NetInterface
  ( -- * Queries
    queryNetInterfaces,
    queryNetInterfacesConfig,

    -- * Functions
    findUp,

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
    IpException (..),
    NmCliException (..),
  )
where

import GHC.OldList qualified as OL
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
    NetInterfaceState (..),
    NetInterfaceType (..),
    NetInterfaces (..),
  )
import Pythia.Services.Types (Device (..), Ipv4Address (..), Ipv6Address (..))
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp

-- | Queries for network interface information with default configuration.
--
-- Throws 'Pythia.Control.Exception.PythiaException'.
--
-- @since 0.1.0.0
queryNetInterfaces :: (MonadCatch m, MonadIO m) => m NetInterfaces
queryNetInterfaces = queryNetInterfacesConfig mempty

-- | Queries for network interface information based on the configuration.
-- If 'interfaceApp' is 'Many' then we try supported apps in the following
-- order: ['NetInterfaceNmCli', 'NetInterfaceIp'].
--
-- Throws 'Pythia.Control.Exception.PythiaException'.
--
-- @since 0.1.0.0
queryNetInterfacesConfig ::
  ( MonadCatch m,
    MonadIO m
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
  ( MonadCatch m,
    MonadIO m
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
    MonadIO m
  ) =>
  NetInterfaceApp ->
  m NetInterfaces
toSingleShellApp NetInterfaceNmCli = NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceIp = Ip.netInterfaceShellApp

-- | Takes the first 'NetInterface' that has state 'Up', according to
-- 'NetInterfaceState'\'s 'Ord':
--
-- @
-- 'Ethernet' < 'Wifi' < 'Wifi_P2P' < 'Loopback' < 'Tun' < 'UnknownType'
-- @
--
-- __Examples__
-- >>> findUp $ MkNetInterfaces []
-- Nothing
--
-- >>> :{
--   let wifiUp = MkNetInterface "" (Just Wifi) Up (Just "WifiUp") [] []
--       wifiDown = MkNetInterface "" (Just Wifi) Down (Just "WifiDown") [] []
--       loopUp = MkNetInterface "" (Just Loopback) Up (Just "LoopUp") [] []
--    in findUp $ MkNetInterfaces [loopUp, wifiDown, wifiUp]
-- :}
-- Just (MkNetInterface {idevice = MkDevice {unDevice = ""}, itype = Just Wifi, istate = Up, iname = Just "WifiUp", ipv4s = [], ipv6s = []})
--
-- @since 0.1.0.0
findUp :: NetInterfaces -> Maybe NetInterface
findUp = headMaybe . (sortType . filterUp) . unNetInterfaces
  where
    sortType = OL.sortOn (view #itype)
    filterUp = filter ((== Up) . view #istate)
