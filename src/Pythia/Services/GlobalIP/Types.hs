{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types to be used for querying the global IP addresses.
--
-- @since 0.1.0.0
module Pythia.Services.GlobalIP.Types
  ( -- * Configuration
    GlobalIpConfig (..),
    GlobalIpApp (..),
    GlobalIpRequest (..),

    -- ** Extra URL sources
    GlobalIpSources (..),
    UrlSource (..),
    urlSourceCmdIso,

    -- * Result
    GlobalIpAddresses (..),
  )
where

import Optics.Core (Iso)
import Optics.Core qualified as O
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Class.Printer qualified as Pretty
import Pythia.Data.Command (Command (..))
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Services.Types.Network (IpType (..), Ipv4Address (..), Ipv6Address (..))

-- | This type determines what program we use to lookup the ip address.
--
-- @since 0.1.0.0
data GlobalIpApp
  = -- | Uses the dig command to perform a DNS lookup. This is generally the
    -- fastest and most reliable.
    --
    -- @since 0.1.0.0
    GlobalIpDig
  | -- | Uses curl to lookup the ip addresses.
    --
    -- @since 0.1.0.0
    GlobalIpCurl
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
    via (Supremum GlobalIpApp)

-- | @since 0.1.0.0
makePrismLabels ''GlobalIpApp

-- | Flag for deciding which IP information we want to look up.
--
-- @since 0.1.0.0
data GlobalIpRequest
  = -- | @since 0.1.0.0
    GlobalIpRequestIpv4
  | -- | @since 0.1.0.0
    GlobalIpRequestIpv6
  | -- | @since 0.1.0.0
    GlobalIpRequestBoth
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
    via (Supremum GlobalIpRequest)

-- | @since 0.1.0.0
makePrismLabels ''GlobalIpRequest

-- | Additional URL source for retrieving IP information. The intended app
-- should not be included (i.e. curl or dig), but any desired flags should
-- be.
--
-- ==== __Examples__
--
-- @
-- (dig): "@resolver1.opendns.com myip.opendns.com"
-- (dig): -4 TXT o-o.myaddr.l.google.com @ns1.google.com
-- (curl): "http://whatismyip.akamai.com/"
-- ...
-- @
--
-- @since 0.1.0.0
type UrlSource :: IpType -> Type
newtype UrlSource a = MkIpvSource
  { -- | @since 0.1.0.0
    unUrlSource :: Text
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving newtype
    ( -- | @since 0.1.0.0
      IsString
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''UrlSource

-- | Isomorphism between 'UrlSource' and 'Command'
--
-- @since 0.1.0.0
urlSourceCmdIso :: Iso (UrlSource a) (UrlSource a) Command Command
urlSourceCmdIso = O.iso (MkCommand . unUrlSource) (MkIpvSource . unCommand)

-- | Additional URL sources.
--
-- @since 0.1.0.0
data GlobalIpSources = MkGlobalIpSources
  { -- | @since 0.1.0.0
    ipv4Sources :: [UrlSource 'Ipv4],
    -- | @since 0.1.0.0
    ipv6Sources :: [UrlSource 'Ipv6]
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
makeFieldLabelsNoPrefix ''GlobalIpSources

-- | @since 0.1.0.0
instance Semigroup GlobalIpSources where
  MkGlobalIpSources x y <> MkGlobalIpSources x' y' =
    MkGlobalIpSources (x <> x') (y <> y')

-- | @since 0.1.0.0
instance Monoid GlobalIpSources where
  mempty = MkGlobalIpSources mempty mempty

-- | Complete configuration for querying global IP addresses.
--
-- >>> mempty @GlobalIpConfig
-- MkGlobalIpConfig {ipApp = Many, ipRequestType = GlobalIpRequestIpv4, ipSources = MkGlobalIpSources {ipv4Sources = [], ipv6Sources = []}}
--
-- @since 0.1.0.0
data GlobalIpConfig = MkGlobalIpConfig
  { -- | @since 0.1.0.0
    ipApp :: RunApp GlobalIpApp,
    -- | @since 0.1.0.0
    ipRequestType :: GlobalIpRequest,
    -- | @since 0.1.0.0
    ipSources :: GlobalIpSources
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''GlobalIpConfig

-- | @since 0.1.0.0
instance Semigroup GlobalIpConfig where
  MkGlobalIpConfig a t s <> MkGlobalIpConfig a' t' s' =
    MkGlobalIpConfig (a <> a') (t <> t') (s <> s')

-- | @since 0.1.0.0
instance Monoid GlobalIpConfig where
  mempty = MkGlobalIpConfig mempty mempty mempty

-- | Combines 'Ipv4Address' and 'Ipv6Address' with the possibility of having both.
-- This is essentially the @These@ types from the @these@ package specialized
-- to our IP addresses, but we have a custom type here so we can give optics
-- instances.
--
-- @since 0.1.0.0
data GlobalIpAddresses
  = -- | @since 0.1.0.0
    GIpv4 Ipv4Address
  | -- | @since 0.1.0.0
    GIpv6 Ipv6Address
  | -- | @since 0.1.0.0
    GIpBoth Ipv4Address Ipv6Address
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makePrismLabels ''GlobalIpAddresses

-- | @since 0.1.0.0
instance PrettyPrinter GlobalIpAddresses where
  pretty (GIpv4 ipv4) = pretty ipv4
  pretty (GIpv6 ipv6) = pretty ipv6
  pretty (GIpBoth ipv4 ipv6) =
    Pretty.joinNewlines
      [ "IPv4: " <> pretty ipv4,
        "IPv6: " <> pretty ipv6
      ]
