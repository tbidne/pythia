{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides network interface types.
--
-- @since 0.1
module Pythia.Services.NetInterface.Types
  ( -- * Configuration
    NetInterfaceApp (..),
    NetInterfaceConfig (..),

    -- * NetInterface Fields
    NetInterfaceType (..),
    NetInterfaceState (..),
    NetInterface (..),
    NetInterfaces (..),

    -- * Optics
    _NetInterfaceAppNmCli,
    _NetInterfaceAppIp,
    _MkNetInterfaceConfig,
    _NetInterfaceTypeEthernet,
    _NetInterfaceTypeWifi,
    _NetInterfaceTypeWifi_P2P,
    _NetInterfaceTypeLoopback,
    _NetInterfaceTypeTun,
    _NetInterfaceStateUp,
    _NetInterfaceStateDown,
    _NetInterfaceStateUnknown,
    _MkNetInterfaces,
  )
where

import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Services.Types.Network
  ( Device,
    IpAddresses (..),
    IpType (..),
  )
import Pythia.Utils (Pretty (..), (<+>))
import Pythia.Utils qualified as U

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
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup
    )
    via (Supremum NetInterfaceApp)
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''NetInterfaceApp

-- | Complete configuration for querying network interfaces.
--
-- >>> mempty @NetInterfaceConfig
-- MkNetInterfaceConfig {app = RunAppMany}
--
-- @since 0.1
type NetInterfaceConfig :: Type
newtype NetInterfaceConfig = MkNetInterfaceConfig
  { -- | @since 0.1
    app :: RunApp NetInterfaceApp
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
makePrisms ''NetInterfaceConfig

-- | @since 0.1
instance Semigroup NetInterfaceConfig where
  MkNetInterfaceConfig a <> MkNetInterfaceConfig a' =
    MkNetInterfaceConfig (a <> a')
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Monoid NetInterfaceConfig where
  mempty = MkNetInterfaceConfig mempty
  {-# INLINEABLE mempty #-}

-- | Various connection types.
--
-- @since 0.1
type NetInterfaceType :: Type
data NetInterfaceType
  = -- | @since 0.1
    NetInterfaceTypeEthernet
  | -- | @since 0.1
    NetInterfaceTypeWifi
  | -- | @since 0.1
    NetInterfaceTypeWifi_P2P
  | -- | @since 0.1
    NetInterfaceTypeLoopback
  | -- | @since 0.1
    NetInterfaceTypeTun
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
makePrisms ''NetInterfaceType

instance Pretty NetInterfaceType where
  pretty = pretty . show
  {-# INLINEABLE pretty #-}

-- | Various connection states.
--
-- @since 0.1
type NetInterfaceState :: Type
data NetInterfaceState
  = -- | @since 0.1
    NetInterfaceStateUp
  | -- | @since 0.1
    NetInterfaceStateDown
  | -- | @since 0.1
    NetInterfaceStateUnknown Text
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
makePrisms ''NetInterfaceState

instance Pretty NetInterfaceState where
  pretty = pretty . show
  {-# INLINEABLE pretty #-}

-- | BatteryStatusFull connection data.
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
    -- | The name of the connection (e.g. NetInterfaceTypeWifi SSID).
    --
    -- @since 0.1
    name :: Maybe Text,
    -- | @since 0.1
    ipv4s :: IpAddresses 'IpTypeIpv4,
    -- | @since 0.1
    ipv6s :: IpAddresses 'IpTypeIpv6
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
makeFieldLabelsNoPrefix ''NetInterface

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
newtype NetInterfaces = MkNetInterfaces
  { -- | @since 0.1
    unNetInterfaces :: [NetInterface]
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
makePrisms ''NetInterfaces

-- | @since 0.1
instance Pretty NetInterfaces where
  pretty = U.vsep . U.punctuate (U.pretty @Text "\n") . fmap pretty . unNetInterfaces
  {-# INLINEABLE pretty #-}
