{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
    DeviceNotFoundException (..),
    IpException (..),
    NmCliException (..),
  )
where

import Data.Text qualified as T
import GHC.OldList qualified as OL
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
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
import Pythia.Services.Types.Network (Device (..), IpAddress (..), IpType (..))
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U

-- | Exception for when we cannot find a desired device.
--
-- @since 0.1
type DeviceNotFoundException :: Type
newtype DeviceNotFoundException = MkDeviceNotFoundException
  { -- | @since 0.1
    unDeviceNotFoundException :: Device
  }
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''DeviceNotFoundException

-- | @since 0.1
instance Pretty DeviceNotFoundException where
  pretty (MkDeviceNotFoundException d) =
    pretty @Text "Device not found: <"
      <> pretty d
      <> pretty @Text ">"
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception DeviceNotFoundException where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | Queries for all network interface data. If the 'NetInterfaceConfig'\'s app
-- is 'Many' then we try all 'NetInterfaceApp's supported by this system, in
-- the following order:
--
-- @
-- ['NetInterfaceNmCli', 'NetInterfaceIp']
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
  Many -> runMultipleQueries
  Single app -> toSingleShellApp app
{-# INLINEABLE queryNetInterfaces #-}

-- | Like 'queryNetInterfaces' but returns data for a single device.
--
-- __Throws:__
--
-- * 'DeviceNotFoundException': if the device is not found.
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryNetInterface :: Device -> NetInterfaceConfig -> IO NetInterface
queryNetInterface d = queryNetInterfaces >=> findDevice d
{-# INLINEABLE queryNetInterface #-}

findDevice :: Device -> NetInterfaces -> IO NetInterface
findDevice device = throwMaybe e . headMaybe . unNetInterfaces . filterDevice device
  where
    e = MkDeviceNotFoundException device
{-# INLINEABLE findDevice #-}

-- | Takes the first 'NetInterface' that has state 'Up', according to
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
--   let wifiUp = MkNetInterface "" (Just Wifi) Up (Just "WifiUp") mempty mempty
--       wifiDown = MkNetInterface "" (Just Wifi) Down (Just "WifiDown") mempty mempty
--       loopUp = MkNetInterface "" (Just Loopback) Up (Just "LoopUp") mempty mempty
--    in findUp $ MkNetInterfaces [loopUp, wifiDown, wifiUp]
-- :}
-- Just (MkNetInterface {device = MkDevice {unDevice = ""}, ntype = Just Wifi, state = Up, name = Just "WifiUp", ipv4s = MkIpAddresses {unIpAddresses = []}, ipv6s = MkIpAddresses {unIpAddresses = []}})
--
-- @since 0.1
findUp :: NetInterfaces -> Maybe NetInterface
findUp = headMaybe . (sortType . filterUp) . unNetInterfaces
  where
    sortType = OL.sortOn (view #ntype)
    filterUp = filter ((== Up) . view #state)
{-# INLINEABLE findUp #-}

runMultipleQueries :: IO NetInterfaces
runMultipleQueries = ShellApp.tryAppActions allApps
  where
    allApps =
      [ MkAppAction (toSingleShellApp NetInterfaceNmCli) NmCli.supported (showt NetInterfaceNmCli),
        MkAppAction (toSingleShellApp NetInterfaceIp) Ip.supported (showt NetInterfaceIp)
      ]
{-# INLINEABLE runMultipleQueries #-}

filterDevice :: Device -> NetInterfaces -> NetInterfaces
filterDevice device (MkNetInterfaces ifs) =
  MkNetInterfaces $
    filter ((== device) . view #device) ifs
{-# INLINEABLE filterDevice #-}

toSingleShellApp :: NetInterfaceApp -> IO NetInterfaces
toSingleShellApp NetInterfaceNmCli = NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceIp = Ip.netInterfaceShellApp
{-# INLINEABLE toSingleShellApp #-}
