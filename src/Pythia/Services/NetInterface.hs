-- | This module exports interface related services.
--
-- @since 0.1
module Pythia.Services.NetInterface
  ( -- * Queries
    queryNetInterfaces,
    queryNetInterface,

    -- * Functions
    findUp,

    -- * Types
    NetInterfaces (..),
    NetInterface (..),
    NetInterfaceState (..),
    NetInterfaceType (..),
    Device (..),
    IpType (..),
    IpAddress (..),

    -- ** Configuration
    NetInterfaceApp (..),
  )
where

import GHC.OldList qualified as OL
import Pythia.Prelude
import Pythia.Services.NetInterface.Ip qualified as Ip
import Pythia.Services.NetInterface.NmCli qualified as NmCli
import Pythia.Services.NetInterface.Types
import Pythia.Services.Types.Network

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude
-- >>> import Pythia.Services.NetInterface.Types (DeviceNotFound)

-- | Queries for all network interface data.
--
-- @since 0.1
queryNetInterfaces ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  NetInterfaceApp ->
  m NetInterfaces
queryNetInterfaces NetInterfaceAppNmCli = NmCli.netInterfaceShellApp
queryNetInterfaces NetInterfaceAppIp = Ip.netInterfaceShellApp
{-# INLINEABLE queryNetInterfaces #-}

-- | Like 'queryNetInterfaces' but returns data for a single device.
--
-- @since 0.1
queryNetInterface ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  Device ->
  NetInterfaceApp ->
  m NetInterface
queryNetInterface d = queryNetInterfaces >=> findDevice d
{-# INLINEABLE queryNetInterface #-}

findDevice :: (HasCallStack, MonadThrow m) => Device -> NetInterfaces -> m NetInterface
findDevice device = throwMaybe e . headMaybe . view #unNetInterfaces . filterDevice device
  where
    e = MkDeviceNotFound device
{-# INLINEABLE findDevice #-}

-- | Takes the first 'NetInterface' that has state 'NetStateUp', according to
-- 'NetInterfaceState'\'s 'Ord':
--
-- @
-- 'Ethernet' < 'Wifi' < 'Wifi_P2P' < 'Loopback' < 'Tun'
-- @
--
-- __Examples__
--
-- >>> findUp $ MkNetInterfaces []
-- Nothing
--
-- >>> :{
--   let wifiUp = MkNetInterface "" (Just Wifi) NetStateUp (Just "WifiUp") mempty mempty
--       wifiNetStateDown = MkNetInterface "" (Just Wifi) NetStateDown (Just "WifiNetStateDown") mempty mempty
--       loopUp = MkNetInterface "" (Just Loopback) NetStateUp (Just "LoopUp") mempty mempty
--    in findUp $ MkNetInterfaces [loopUp, wifiNetStateDown, wifiUp]
-- :}
-- Just (MkNetInterface {device = MkDevice {unDevice = ""}, ntype = Just Wifi, state = NetStateUp, name = Just "WifiUp", ipv4s = MkIpAddresses {unIpAddresses = []}, ipv6s = MkIpAddresses {unIpAddresses = []}})
--
-- @since 0.1
findUp :: NetInterfaces -> Maybe NetInterface
findUp = headMaybe . (sortType . filterUp) . view #unNetInterfaces
  where
    sortType = OL.sortOn (view #ntype)
    filterUp = filter ((== NetStateUp) . view #state)
{-# INLINEABLE findUp #-}

filterDevice :: Device -> NetInterfaces -> NetInterfaces
filterDevice device (MkNetInterfaces ifs) =
  MkNetInterfaces
    $ filter ((== device) . view #device) ifs
{-# INLINEABLE filterDevice #-}
