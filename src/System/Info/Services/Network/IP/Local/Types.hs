{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides type for local IP addresses.
--
-- @since 0.1.0.0
module System.Info.Services.Network.IP.Local.Types
  ( Ipv4,
    Ipv6,
    LocalIpAddresses (..),
    LocalIps (..),
  )
where

import Optics.TH qualified as OTH
import System.Info.Services.Network.IP.Types (Ipv4, Ipv6)
import System.Info.Services.Network.Types (Device)

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

-- | @since 0.1.0.0
instance Semigroup LocalIpAddresses where
  MkLocalIpAddresses v4 v6 <> MkLocalIpAddresses v4' v6' = MkLocalIpAddresses (v4 <> v4') (v6 <> v6')

-- | @since 0.1.0.0
instance Monoid LocalIpAddresses where
  mempty = MkLocalIpAddresses mempty mempty

OTH.makeFieldLabelsNoPrefix ''LocalIpAddresses

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

OTH.makeFieldLabelsNoPrefix ''LocalIps
