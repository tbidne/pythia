{-# LANGUAGE DeriveAnyClass #-}
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
  )
where

import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Class.Printer qualified as Printer
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Services.Types.Network (Device, Ipv4Address, Ipv6Address)

-- | Determines how we should query the system for interface state information.
--
-- @since 0.1
data NetInterfaceApp
  = -- | Uses the Network Manager cli utility.
    --
    -- @since 0.1
    NetInterfaceNmCli
  | -- | Uses the \'ip\' utility.
    --
    -- @since 0.1
    NetInterfaceIp
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
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

-- | @since 0.1
makePrismLabels ''NetInterfaceApp

-- | Complete configuration for querying network interfaces.
--
-- >>> mempty @NetInterfaceConfig
-- MkNetInterfaceConfig {interfaceApp = Many, interfaceDevice = Nothing}
--
-- @since 0.1
data NetInterfaceConfig = MkNetInterfaceConfig
  { -- | @since 0.1
    interfaceApp :: RunApp NetInterfaceApp,
    -- | @since 0.1
    interfaceDevice :: Maybe Device
  }
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''NetInterfaceConfig

-- | @since 0.1
instance Semigroup NetInterfaceConfig where
  MkNetInterfaceConfig a d <> MkNetInterfaceConfig a' d' =
    MkNetInterfaceConfig (a <> a') (d <|> d')

-- | @since 0.1
instance Monoid NetInterfaceConfig where
  mempty = MkNetInterfaceConfig mempty empty

-- | Various connection types.
--
-- @since 0.1
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
  | -- | @since 0.1
    UnknownType Text
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      PrettyPrinter
    )

-- | @since 0.1
makePrismLabels ''NetInterfaceType

-- | Various connection states.
--
-- @since 0.1
data NetInterfaceState
  = -- | @since 0.1
    Up
  | -- | @since 0.1
    Down
  | -- | @since 0.1
    UnknownState Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      PrettyPrinter
    )

-- | @since 0.1
makePrismLabels ''NetInterfaceState

-- | Full connection data.
--
-- @since 0.1
data NetInterface = MkNetInterface
  { -- | @since 0.1
    idevice :: Device,
    -- | @since 0.1
    itype :: Maybe NetInterfaceType,
    -- | @since 0.1
    istate :: NetInterfaceState,
    -- | The name of the connection (e.g. Wifi SSID).
    --
    -- @since 0.1
    iname :: Maybe Text,
    -- | @since 0.1
    ipv4s :: [Ipv4Address],
    -- | @since 0.1
    ipv6s :: [Ipv6Address]
  }
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''NetInterface

-- | @since 0.1
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

-- | @since 0.1
newtype NetInterfaces = MkNetInterfaces {unNetInterfaces :: [NetInterface]}
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance PrettyPrinter NetInterfaces where
  pretty = Printer.joinX "\n\n" . unNetInterfaces

-- | @since 0.1
makeFieldLabelsNoPrefix ''NetInterfaces
