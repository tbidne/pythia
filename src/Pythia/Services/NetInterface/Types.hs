{-# LANGUAGE UndecidableInstances #-}

-- | Provides network interface types.
--
-- @since 0.1
module Pythia.Services.NetInterface.Types
  ( -- * Configuration
    NetInterfaceApp (..),

    -- * NetInterface Fields
    NetInterfaceType (..),
    NetInterfaceState (..),
    NetInterface (..),
    NetInterfaces (..),

    -- * Errors
    DeviceNotFound (..),

    -- * Optics
    _NetInterfaceAppNmCli,
    _NetInterfaceAppIp,
    _Ethernet,
    _Wifi,
    _Wifi_P2P,
    _Loopback,
    _Tun,
    _NetStateUp,
    _NetStateDown,
    _NetStateUnknown,
  )
where

import Data.Text qualified as T
import Pythia.Prelude
import Pythia.Services.Types.Network
  ( Device,
    IpAddresses (..),
    IpType (..),
  )
import Pythia.Utils (Pretty (..), (<+>))
import Pythia.Utils qualified as U

-- $setup
-- >>> import Pythia.Prelude

-- | Determines how we should query the system for interface state information.
--
-- @since 0.1
type NetInterfaceApp :: Type
data NetInterfaceApp
  = -- | Uses the Network Manager cli utility.
    --
    -- @since 0.1
    NetInterfaceAppNmCli
  | -- | Uses the \'ip\' utility.
    --
    -- @since 0.1
    NetInterfaceAppIp
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_NetInterfaceAppIp :: Prism' NetInterfaceApp ()
_NetInterfaceAppIp =
  prism
    (const NetInterfaceAppIp)
    ( \x -> case x of
        NetInterfaceAppIp -> Right ()
        _ -> Left x
    )
{-# INLINE _NetInterfaceAppIp #-}

-- | @since 0.1
_NetInterfaceAppNmCli :: Prism' NetInterfaceApp ()
_NetInterfaceAppNmCli =
  prism
    (const NetInterfaceAppNmCli)
    ( \x -> case x of
        NetInterfaceAppNmCli -> Right ()
        _ -> Left x
    )
{-# INLINE _NetInterfaceAppNmCli #-}

-- | Various connection types.
--
-- @since 0.1
type NetInterfaceType :: Type
data NetInterfaceType
  = -- | @since 0.1
    Ethernet
  | -- | @since 0.1
    Wifi
  | -- | @since 0.1
    Wifi_P2P
  | -- | @since 0.1
    Loopback
  | -- | @since 0.1
    Tun
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_Ethernet :: Prism' NetInterfaceType ()
_Ethernet =
  prism
    (const Ethernet)
    ( \x -> case x of
        Ethernet -> Right ()
        _ -> Left x
    )
{-# INLINE _Ethernet #-}

-- | @since 0.1
_Wifi :: Prism' NetInterfaceType ()
_Wifi =
  prism
    (const Wifi)
    ( \x -> case x of
        Wifi -> Right ()
        _ -> Left x
    )
{-# INLINE _Wifi #-}

-- | @since 0.1
_Wifi_P2P :: Prism' NetInterfaceType ()
_Wifi_P2P =
  prism
    (const Wifi_P2P)
    ( \x -> case x of
        Wifi_P2P -> Right ()
        _ -> Left x
    )
{-# INLINE _Wifi_P2P #-}

-- | @since 0.1
_Loopback :: Prism' NetInterfaceType ()
_Loopback =
  prism
    (const Loopback)
    ( \x -> case x of
        Loopback -> Right ()
        _ -> Left x
    )
{-# INLINE _Loopback #-}

-- | @since 0.1
_Tun :: Prism' NetInterfaceType ()
_Tun =
  prism
    (const Tun)
    ( \x -> case x of
        Tun -> Right ()
        _ -> Left x
    )
{-# INLINE _Tun #-}

instance Pretty NetInterfaceType where
  pretty = pretty . show
  {-# INLINEABLE pretty #-}

-- | Various connection states.
--
-- @since 0.1
type NetInterfaceState :: Type
data NetInterfaceState
  = -- | @since 0.1
    NetStateUp
  | -- | @since 0.1
    NetStateDown
  | -- | @since 0.1
    NetStateUnknown Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_NetStateUp :: Prism' NetInterfaceState ()
_NetStateUp =
  prism
    (const NetStateUp)
    ( \x -> case x of
        NetStateUp -> Right ()
        _ -> Left x
    )
{-# INLINE _NetStateUp #-}

-- | @since 0.1
_NetStateDown :: Prism' NetInterfaceState ()
_NetStateDown =
  prism
    (const NetStateDown)
    ( \x -> case x of
        NetStateDown -> Right ()
        _ -> Left x
    )
{-# INLINE _NetStateDown #-}

-- | @since 0.1
_NetStateUnknown :: Prism' NetInterfaceState Text
_NetStateUnknown =
  prism
    NetStateUnknown
    ( \x -> case x of
        NetStateUnknown t -> Right t
        _ -> Left x
    )
{-# INLINE _NetStateUnknown #-}

instance Pretty NetInterfaceState where
  pretty = pretty . show
  {-# INLINEABLE pretty #-}

-- | Full connection data.
--
-- @since 0.1
type NetInterface :: Type
data NetInterface = MkNetInterface
  { -- | @since 0.1
    device :: Device,
    -- | @since 0.1
    ntype :: Maybe NetInterfaceType,
    -- | @since 0.1
    state :: NetInterfaceState,
    -- | The name of the connection (e.g. Wifi SSID).
    --
    -- @since 0.1
    name :: Maybe Text,
    -- | @since 0.1
    ipv4s :: IpAddresses Ipv4,
    -- | @since 0.1
    ipv6s :: IpAddresses Ipv6
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Device, b ~ Device) =>
  LabelOptic "device" k NetInterface NetInterface a b
  where
  labelOptic = lensVL $ \f (MkNetInterface _device _ntype _state _name _ipv4s _ipv6s) ->
    fmap (\device' -> MkNetInterface device' _ntype _state _name _ipv4s _ipv6s) (f _device)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NetInterfaceType, b ~ Maybe NetInterfaceType) =>
  LabelOptic "ntype" k NetInterface NetInterface a b
  where
  labelOptic = lensVL $ \f (MkNetInterface _device _ntype _state _name _ipv4s _ipv6s) ->
    fmap (\ntype' -> MkNetInterface _device ntype' _state _name _ipv4s _ipv6s) (f _ntype)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ NetInterfaceState, b ~ NetInterfaceState) =>
  LabelOptic "state" k NetInterface NetInterface a b
  where
  labelOptic = lensVL $ \f (MkNetInterface _device _ntype _state _name _ipv4s _ipv6s) ->
    fmap (\state' -> MkNetInterface _device _ntype state' _name _ipv4s _ipv6s) (f _state)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "name" k NetInterface NetInterface a b
  where
  labelOptic = lensVL $ \f (MkNetInterface _device _ntype _state _name _ipv4s _ipv6s) ->
    fmap (\name' -> MkNetInterface _device _ntype _state name' _ipv4s _ipv6s) (f _name)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ IpAddresses Ipv4, b ~ IpAddresses Ipv4) =>
  LabelOptic "ipv4s" k NetInterface NetInterface a b
  where
  labelOptic = lensVL $ \f (MkNetInterface _device _ntype _state _name _ipv4s _ipv6s) ->
    fmap (\ipv4s' -> MkNetInterface _device _ntype _state _name ipv4s' _ipv6s) (f _ipv4s)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ IpAddresses Ipv6, b ~ IpAddresses Ipv6) =>
  LabelOptic "ipv6s" k NetInterface NetInterface a b
  where
  labelOptic = lensVL $ \f (MkNetInterface _device _ntype _state _name _ipv4s _ipv6s) ->
    fmap (MkNetInterface _device _ntype _state _name _ipv4s) (f _ipv6s)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Pretty NetInterface where
  pretty netif =
    U.vsep
      [ device,
        ctype,
        state,
        name,
        ipv4s,
        ipv6s
      ]
    where
      device = "Device:" <+> pretty (netif ^. #device)
      ctype = "Type:" <+> pretty (netif ^. #ntype)
      state = "State:" <+> pretty (netif ^. #state)
      name = "Name:" <+> pretty (netif ^. #name)
      ipv4s = "IPv4:" <+> pretty (netif ^. #ipv4s)
      ipv6s = "IPv6:" <+> pretty (netif ^. #ipv6s)
  {-# INLINEABLE pretty #-}

-- | @since 0.1
type NetInterfaces :: Type
newtype NetInterfaces = MkNetInterfaces {unNetInterfaces :: [NetInterface]}
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Pretty NetInterfaces where
  pretty = U.vsep . U.punctuate (U.pretty @Text "\n") . fmap pretty . view #unNetInterfaces
  {-# INLINEABLE pretty #-}

-- | Exception for when we cannot find a desired device.
--
-- ==== __Examples__
--
-- >>> displayException $ MkDeviceNotFound "bad device"
-- "Device not found: bad device"
--
-- @since 0.1
type DeviceNotFound :: Type
newtype DeviceNotFound = MkDeviceNotFound Device
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
instance Exception DeviceNotFound where
  displayException (MkDeviceNotFound d) =
    ("Device not found: " <>)
      . T.unpack
      . view #unDevice
      $ d
