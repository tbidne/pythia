{-# LANGUAGE TemplateHaskell #-}

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
    NetInterfaceConfig (..),
    NetInterfaceApp (..),
    RunApp (..),

    -- ** Errors
    DeviceNotFound (..),
    IpParseError (..),
    NmCliParseError (..),

    -- * Optics

    -- ** Types
    _MkNetInterfaces,
    _NetInterfaceStateUp,
    _NetInterfaceStateDown,
    _NetInterfaceStateUnknown,
    _NetInterfaceTypeEthernet,
    _NetInterfaceTypeWifi,
    _NetInterfaceTypeWifi_P2P,
    _NetInterfaceTypeLoopback,
    _NetInterfaceTypeTun,
    _MkDevice,
    _IpTypeIpv4,
    _IpTypeIpv6,
    _MkIpAddress,

    -- ** Configuration
    _MkNetInterfaceConfig,
    _NetInterfaceAppNmCli,
    _NetInterfaceAppIp,

    -- ** Errors
    _MkDeviceNotFound,
    _MkIpParseError,
    _MkNmCliParseError,
  )
where

import Data.Text qualified as T
import GHC.OldList qualified as OL
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Internal.ShellApp (AppAction (..))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.NetInterface.Ip
import Pythia.Services.NetInterface.Ip qualified as Ip
import Pythia.Services.NetInterface.NmCli
import Pythia.Services.NetInterface.NmCli qualified as NmCli
import Pythia.Services.NetInterface.Types
import Pythia.Services.Types.Network
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U

-- | Exception for when we cannot find a desired device.
--
-- ==== __Examples__
--
-- >>> displayException $ MkDeviceNotFound "bad device"
-- "Device not found: bad device"
--
-- @since 0.1
type DeviceNotFound :: Type
newtype DeviceNotFound = MkDeviceNotFound
  { -- | @since 0.1
    unDeviceNotFound :: Device
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''DeviceNotFound

-- | @since 0.1
instance Pretty DeviceNotFound where
  pretty (MkDeviceNotFound d) =
    pretty @Text "Device not found: "
      <> pretty d
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception DeviceNotFound where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | Queries for all network interface data. If the 'NetInterfaceConfig'\'s app
-- is 'RunAppMany' then we try all 'NetInterfaceApp's supported by this system, in
-- the following order:
--
-- @
-- ['NetInterfaceAppNmCli', 'NetInterfaceAppIp']
-- @
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryNetInterfaces :: NetInterfaceConfig -> IO NetInterfaces
queryNetInterfaces cfg = case cfg ^. #app of
  RunAppMany -> runMultipleQueries
  RunAppSingle app -> toSingleShellApp app
{-# INLINEABLE queryNetInterfaces #-}

-- | Like 'queryNetInterfaces' but returns data for a single device.
--
-- __Throws:__
--
-- * 'DeviceNotFound'
-- * 'IpParseError'
-- * 'NmCliParseError'
-- * 'Pythia.Control.Exception.CommandException'
--
-- @since 0.1
queryNetInterface :: Device -> NetInterfaceConfig -> IO NetInterface
queryNetInterface d = queryNetInterfaces >=> findDevice d
{-# INLINEABLE queryNetInterface #-}

findDevice :: Device -> NetInterfaces -> IO NetInterface
findDevice device = throwMaybe e . headMaybe . unNetInterfaces . filterDevice device
  where
    e = MkDeviceNotFound device
{-# INLINEABLE findDevice #-}

-- | Takes the first 'NetInterface' that has state 'NetInterfaceStateUp', according to
-- 'NetInterfaceState'\'s 'Ord':
--
-- @
-- 'NetInterfaceTypeEthernet' < 'NetInterfaceTypeWifi' < 'NetInterfaceTypeWifi_P2P' < 'NetInterfaceTypeLoopback' < 'NetInterfaceTypeTun'
-- @
--
-- __Examples__
--
-- >>> findUp $ MkNetInterfaces []
-- Nothing
--
-- >>> :{
--   let wifiUp = MkNetInterface "" (Just NetInterfaceTypeWifi) NetInterfaceStateUp (Just "NetInterfaceTypeWifiUp") mempty mempty
--       wifiNetInterfaceStateDown = MkNetInterface "" (Just NetInterfaceTypeWifi) NetInterfaceStateDown (Just "NetInterfaceTypeWifiNetInterfaceStateDown") mempty mempty
--       loopUp = MkNetInterface "" (Just NetInterfaceTypeLoopback) NetInterfaceStateUp (Just "LoopUp") mempty mempty
--    in findUp $ MkNetInterfaces [loopUp, wifiNetInterfaceStateDown, wifiUp]
-- :}
-- Just (MkNetInterface {device = MkDevice {unDevice = ""}, ntype = Just NetInterfaceTypeWifi, state = NetInterfaceStateUp, name = Just "NetInterfaceTypeWifiUp", ipv4s = MkIpAddresses {unIpAddresses = []}, ipv6s = MkIpAddresses {unIpAddresses = []}})
--
-- @since 0.1
findUp :: NetInterfaces -> Maybe NetInterface
findUp = headMaybe . (sortType . filterUp) . unNetInterfaces
  where
    sortType = OL.sortOn (view #ntype)
    filterUp = filter ((== NetInterfaceStateUp) . view #state)
{-# INLINEABLE findUp #-}

runMultipleQueries :: IO NetInterfaces
runMultipleQueries = ShellApp.tryAppActions allApps
  where
    allApps =
      [ MkAppAction (toSingleShellApp NetInterfaceAppNmCli) NmCli.supported "nmcli",
        MkAppAction (toSingleShellApp NetInterfaceAppIp) Ip.supported "ip"
      ]
{-# INLINEABLE runMultipleQueries #-}

filterDevice :: Device -> NetInterfaces -> NetInterfaces
filterDevice device (MkNetInterfaces ifs) =
  MkNetInterfaces $
    filter ((== device) . view #device) ifs
{-# INLINEABLE filterDevice #-}

toSingleShellApp :: NetInterfaceApp -> IO NetInterfaces
toSingleShellApp NetInterfaceAppNmCli = NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceAppIp = Ip.netInterfaceShellApp
{-# INLINEABLE toSingleShellApp #-}
