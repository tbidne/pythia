{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types to be used for querying the global IP addresses.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Global.Types
  ( -- * IP Types
    Ipv4 (..),
    Ipv6 (..),
    GlobalIpAddresses (..),

    -- * Custom Commands
    Ipv4Command (..),
    ipv4CmdIso,
    Ipv6Command (..),
    ipv6CmdIso,
    GlobalIpCommand (..),

    -- * Other
    IpStrategy (..),
  )
where

import Data.String (IsString)
import Optics.Core (Iso)
import Optics.Core qualified as O
import Optics.TH qualified as OTH
import Pythia.Data (Command (..))
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Printer qualified as Pretty
import Pythia.Services.Network.IP.Types (Ipv4 (..), Ipv6 (..))

-- | Combines 'Ipv4' and 'Ipv6' with the possibility of having both.
-- This is essentially the @These@ types from the @these@ package specialized
-- to our IP addresses, but we have a custom type here so we can give optics
-- instances.
--
-- @since 0.1.0.0
data GlobalIpAddresses
  = -- | @since 0.1.0.0
    GIpv4 Ipv4
  | -- | @since 0.1.0.0
    GIpv6 Ipv6
  | -- | @since 0.1.0.0
    GIpBoth Ipv4 Ipv6
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter GlobalIpAddresses where
  pretty (GIpv4 ipv4) = "IPv4: " <> pretty ipv4
  pretty (GIpv6 ipv6) = "IPv6: " <> pretty ipv6
  pretty (GIpBoth ipv4 ipv6) =
    Pretty.joinNewlines
      [ "IPv4: " <> pretty ipv4,
        "IPv6: " <> pretty ipv6
      ]

-- | This type determines what strategy we will use when looking up
-- the IP address. We must use an external source to determine our
-- "global" IP address, so 'IpStrategy' is concerned with what sources
-- we use.
--
-- The default (i.e. 'Monoid' identity) is 'Defaults'.
--
-- @since 0.1.0.0
data IpStrategy
  = -- | Uses predetermined sources (i.e. server URLS) to look up our IP
    -- address.
    --
    -- @since 0.1.0.0
    Defaults
  | -- | Queries for IP information based on the parameter 'GlobalIpCommand'.
    --
    -- @since 0.1.0.0
    CustomUrl GlobalIpCommand
  | -- | Queries for IP information based on the parameter 'GlobalIpCommand'.
    -- If that fails, falls back to the defaults.
    --
    -- @since 0.1.0.0
    CustomWithDefaults GlobalIpCommand
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance Semigroup IpStrategy where
  Defaults <> r = r
  l <> Defaults = l
  CustomWithDefaults _ <> CustomUrl t = CustomUrl t
  l <> _ = l

-- | @since 0.1.0.0
instance Monoid IpStrategy where
  mempty = Defaults

-- | Newtype wrapper over a shell command for retrieving a global IPv4
-- address. This should be the full command, e.g.,
--
-- @
-- "dig @resolver1.opendns.com myip.opendns.com +short"
-- "curl http://whatismyip.akamai.com/"
-- ...
-- @
--
-- @since 0.1.0.0
newtype Ipv4Command = MkIpv4Command
  { -- | @since 0.1.0.0
    unIpv4Command :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      IsString
    )
    via Text

-- | 'Iso' for 'Ipv4Command' and 'Command'.
--
-- @since 0.1.0.0
ipv4CmdIso :: Iso Ipv4Command Ipv4Command Command Command
ipv4CmdIso = O.iso (MkCommand . unIpv4Command) (MkIpv4Command . unCommand)

-- | Newtype wrapper over a shell command for retrieving a global IPv6
-- address.
--
-- @since 0.1.0.0
newtype Ipv6Command = MkIpv6Command
  { -- | @since 0.1.0.0
    unIpv6Command :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      IsString
    )
    via Text

-- | 'Iso' for 'Ipv6Command' and 'Command'.
--
-- @since 0.1.0.0
ipv6CmdIso :: Iso Ipv6Command Ipv6Command Command Command
ipv6CmdIso = O.iso (MkCommand . unIpv6Command) (MkIpv6Command . unCommand)

-- | This type holds external sources (i.e. servers URLs) used for retrieving
-- global IP addresses (IPv4, IPv6, or both).
--
-- @since 0.1.0.0
data GlobalIpCommand
  = -- | @since 0.1.0.0
    GIpv4Command [Ipv4Command]
  | -- | @since 0.1.0.0
    GIpv6Command [Ipv6Command]
  | -- | @since 0.1.0.0
    GIpBothCommand [Ipv4Command] [Ipv6Command]
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

OTH.makeFieldLabelsNoPrefix ''Ipv6Command
OTH.makeFieldLabelsNoPrefix ''Ipv4Command
OTH.makePrismLabels ''GlobalIpCommand
OTH.makePrismLabels ''GlobalIpAddresses
OTH.makePrismLabels ''IpStrategy
