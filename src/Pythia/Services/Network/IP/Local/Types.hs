{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides type for local IP addresses.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Local.Types
  ( Ipv4 (..),
    Ipv6 (..),
    LocalIpAddresses (..),
    LocalIps (..),
  )
where

import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Printer qualified as Pretty
import Pythia.Printer qualified as Printer
import Pythia.Services.Network.IP.Types (Ipv4 (..), Ipv6 (..))
import Pythia.Services.Network.Types (Device)

-- | Combines multiple Ipv4 and Ipv6 addresses.
--
-- @since 0.1.0.0
data LocalIpAddresses = MkLocalIpAddresses
  { -- | @since 0.1.0.0
    ipv4s :: [Ipv4],
    -- | @since 0.1.0.0
    ipv6s :: [Ipv6]
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

makeFieldLabelsNoPrefix ''LocalIpAddresses

-- | @since 0.1.0.0
instance PrettyPrinter LocalIpAddresses where
  pretty addresses = Printer.joinNewlines [ipv4, ipv6]
    where
      ipv4 = "IPv4: " <> Printer.joinCommas (addresses ^. #ipv4s)
      ipv6 = "IPv6: " <> Printer.joinCommas (addresses ^. #ipv6s)

-- | @since 0.1.0.0
instance Semigroup LocalIpAddresses where
  MkLocalIpAddresses v4 v6 <> MkLocalIpAddresses v4' v6' = MkLocalIpAddresses (v4 <> v4') (v6 <> v6')

-- | @since 0.1.0.0
instance Monoid LocalIpAddresses where
  mempty = MkLocalIpAddresses mempty mempty

-- | Describes the Ipv4 and Ipv6 addresses associated to a device.
--
-- @since 0.1.0.0
data LocalIps = MkLocalIps
  { -- | @since 0.1.0.0
    localDevice :: Device,
    -- | @since 0.1.0.0
    addresses :: LocalIpAddresses
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

makeFieldLabelsNoPrefix ''LocalIps

-- | @since 0.1.0.0
instance PrettyPrinter LocalIps where
  pretty li =
    Pretty.joinNewlines
      [ "Device: " <> pretty (li ^. #localDevice),
        pretty $ li ^. #addresses
      ]
