{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types to be used for querying the global IP addresses.
--
-- @since 0.1
module Pythia.Services.GlobalIp.Types
  ( -- * Configuration
    GlobalIpv4Config,
    GlobalIpv6Config,
    GlobalIpBothConfig,
    GlobalIpConfig (..),
    GlobalIpApp (..),

    -- ** Extra URL sources
    UrlSource (..),

    -- * Optics
    _GlobalIpAppDig,
    _GlobalIpAppCurl,
    _MkUrlSource,
  )
where

import Pythia.Data.RunApp (RunApp (..))
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Services.Types.Network (IpType (..))

-- | This type determines what program we use to lookup the ip address.
--
-- @since 0.1
type GlobalIpApp :: Type
data GlobalIpApp
  = -- | Uses the dig command to perform a DNS lookup. This is generally the
    -- fastest and most reliable.
    --
    -- @since 0.1
    GlobalIpAppDig
  | -- | Uses curl to lookup the ip addresses.
    --
    -- @since 0.1
    GlobalIpAppCurl
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
    via (Supremum GlobalIpApp)
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''GlobalIpApp

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
-- @since 0.1
type UrlSource :: IpType -> Type
newtype UrlSource a = MkUrlSource
  { -- | @since 0.1
    unUrlSource :: Text
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
  deriving newtype
    ( -- | @since 0.1
      IsString
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''UrlSource

-- | Complete configuration for querying global IP addresses. The 'Monoid'
-- instance will construct a config that tries all apps and has no extra
-- sources.
--
-- >>> mempty @(GlobalIpConfig [UrlSource IpTypeIpv4])
-- MkGlobalIpConfig {app = RunAppMany, sources = []}
--
-- @since 0.1
type GlobalIpConfig :: Type -> Type
data GlobalIpConfig a = MkGlobalIpConfig
  { -- | Determines how we want to query.
    --
    -- @since 0.1
    app :: RunApp GlobalIpApp,
    -- | Extra lookup sources. This will be either a single @['UrlSource' a]@
    -- or a pair @(['UrlSource' 'IpTypeIpv4'], ['UrlSource' 'IpTypeIpv6'])@, depending on
    -- which address we want to retrieve.
    --
    -- @since 0.1
    sources :: a
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
makeFieldLabelsNoPrefix ''GlobalIpConfig

-- | @since 0.1
instance Semigroup a => Semigroup (GlobalIpConfig a) where
  MkGlobalIpConfig a s <> MkGlobalIpConfig a' s' =
    MkGlobalIpConfig (a <> a') (s <> s')
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Monoid a => Monoid (GlobalIpConfig a) where
  mempty = MkGlobalIpConfig mempty mempty
  {-# INLINEABLE mempty #-}

-- | Type alias for 'IpTypeIpv4' 'GlobalIpConfig'.
--
-- @since 0.1.0.0
type GlobalIpv4Config :: Type
type GlobalIpv4Config = GlobalIpConfig [UrlSource 'IpTypeIpv4]

-- | Type alias for 'IpTypeIpv6' 'GlobalIpConfig'.
--
-- @since 0.1.0.0
type GlobalIpv6Config :: Type
type GlobalIpv6Config = GlobalIpConfig [UrlSource 'IpTypeIpv6]

-- | Type alias for 'IpTypeIpv4' and 'IpTypeIpv6' 'GlobalIpConfig'.
--
-- @since 0.1.0.0
type GlobalIpBothConfig :: Type
type GlobalIpBothConfig = GlobalIpConfig ([UrlSource 'IpTypeIpv4], [UrlSource 'IpTypeIpv6])
