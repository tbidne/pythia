{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides network interface types.
--
-- @since 0.1.0.0
module Pythia.Services.Network.NetInterface.Types
  ( -- * Configuration
    NetInterfaceApp (..),
    NetInterfaceConfig (..),

    -- * NetInterface Fields
    NetInterfaceType (..),
    NetInterfaceState (..),
    NetInterface (..),
    NetInterfaces (..),
  )
where

import Pythia.Data (RunApp)
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Printer qualified as Printer
import Pythia.Services.Network.Types (Device, Ipv4Address, Ipv6Address)
import Pythia.Supremum (Supremum (..))

-- | Determines how we should query the system for interface state information.
--
-- @since 0.1.0.0
data NetInterfaceApp
  = -- | Uses the Network Manager cli utility.
    --
    -- @since 0.1.0.0
    NetInterfaceNmCli
  | -- | Uses the \'ip\' utility.
    --
    -- @since 0.1.0.0
    NetInterfaceIp
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Enum,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Monoid,
      -- | @since 0.1.0.0
      Semigroup
    )
    via (Supremum NetInterfaceApp)

-- | @since 0.1.0.0
makePrismLabels ''NetInterfaceApp

-- | Complete configuration for querying network interfaces.
--
-- >>> mempty @NetInterfaceConfig
-- MkNetInterfaceConfig {interfaceApp = Many, interfaceDevice = Nothing}
--
-- @since 0.1.0.0
data NetInterfaceConfig = MkNetInterfaceConfig
  { -- | @since 0.1.0.0
    interfaceApp :: RunApp NetInterfaceApp,
    -- | @since 0.1.0.0
    interfaceDevice :: Maybe Device
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''NetInterfaceConfig

-- | @since 0.1.0.0
instance Semigroup NetInterfaceConfig where
  MkNetInterfaceConfig a d <> MkNetInterfaceConfig a' d' =
    MkNetInterfaceConfig (a <> a') (d <|> d')

-- | @since 0.1.0.0
instance Monoid NetInterfaceConfig where
  mempty = MkNetInterfaceConfig mempty empty

-- | Various connection types.
--
-- @since 0.1.0.0
data NetInterfaceType
  = -- | @since 0.1.0.0
    Ethernet
  | -- | @since 0.1.0.0
    Wifi
  | -- | @since 0.1.0.0
    Wifi_P2P
  | -- | @since 0.1.0.0
    Loopback
  | -- | @since 0.1.0.0
    Tun
  | -- | @since 0.1.0.0
    UnknownType Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      PrettyPrinter
    )

-- | @since 0.1.0.0
makePrismLabels ''NetInterfaceType

-- | Various connection states.
--
-- @since 0.1.0.0
data NetInterfaceState
  = -- | @since 0.1.0.0
    Up
  | -- | @since 0.1.0.0
    Down
  | -- | @since 0.1.0.0
    UnknownState Text
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      PrettyPrinter
    )

-- | @since 0.1.0.0
makePrismLabels ''NetInterfaceState

-- | Full connection data.
--
-- @since 0.1.0.0
data NetInterface = MkNetInterface
  { -- | @since 0.1.0.0
    idevice :: Device,
    -- | @since 0.1.0.0
    itype :: Maybe NetInterfaceType,
    -- | @since 0.1.0.0
    istate :: NetInterfaceState,
    -- | The name of the connection (e.g. Wifi SSID).
    --
    -- @since 0.1.0.0
    iname :: Maybe Text,
    -- | @since 0.1.0.0
    ipv4s :: [Ipv4Address],
    -- | @since 0.1.0.0
    ipv6s :: [Ipv6Address]
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''NetInterface

-- | @since 0.1.0.0
instance PrettyPrinter NetInterface where
  pretty netif =
    Printer.joinNewlines
      [ device,
        ctype,
        state,
        name,
        ipv4s,
        ipv6s
      ]
    where
      device = "Device: " <> pretty (netif ^. #idevice)
      ctype = "Type: " <> pretty (netif ^. #itype)
      state = "State: " <> pretty (netif ^. #istate)
      name = "Name: " <> pretty (netif ^. #iname)
      ipv4s = "IPv4: " <> Printer.joinCommas (netif ^. #ipv4s)
      ipv6s = "IPv6: " <> Printer.joinCommas (netif ^. #ipv6s)

-- | @since 0.1.0.0
newtype NetInterfaces = MkNetInterfaces {unNetInterfaces :: [NetInterface]}
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter NetInterfaces where
  pretty = Printer.joinX "\n\n" . unNetInterfaces

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''NetInterfaces
