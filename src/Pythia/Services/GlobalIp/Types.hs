{-# LANGUAGE UndecidableInstances #-}

-- | Provides types to be used for querying the global IP addresses.
--
-- @since 0.1
module Pythia.Services.GlobalIp.Types
  ( -- * Configuration
    GlobalIpApp (..),

    -- ** Extra URL sources
    UrlSource (..),

    -- * Optics
    _GlobalIpAppDig,
    _GlobalIpAppCurl,
  )
where

import Pythia.Prelude
import Pythia.Services.Types.Network (IpType)

-- $setup
-- >>> import Pythia.Prelude
-- >>> import Pythia.Services.Types.Network (IpType (..))

-- | This type determines what program we use to lookup the ip address.
--
-- @since 0.1
type GlobalIpApp :: Type
data GlobalIpApp
  = -- | Uses curl to lookup the ip addresses.
    --
    -- @since 0.1
    GlobalIpAppCurl
  | -- | Uses the dig command to perform a DNS lookup. This is generally the
    -- fastest and most reliable.
    --
    -- @since 0.1
    GlobalIpAppDig
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
_GlobalIpAppCurl :: Prism' GlobalIpApp ()
_GlobalIpAppCurl =
  prism
    (const GlobalIpAppCurl)
    ( \x -> case x of
        GlobalIpAppCurl -> Right ()
        _ -> Left x
    )
{-# INLINE _GlobalIpAppCurl #-}

-- | @since 0.1
_GlobalIpAppDig :: Prism' GlobalIpApp ()
_GlobalIpAppDig =
  prism
    (const GlobalIpAppDig)
    ( \x -> case x of
        GlobalIpAppDig -> Right ()
        _ -> Left x
    )
{-# INLINE _GlobalIpAppDig #-}

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
newtype UrlSource a = MkUrlSource {unUrlSource :: Text}
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
instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unUrlSource" k (UrlSource p) (UrlSource p) a b
  where
  labelOptic = iso (\(MkUrlSource s) -> s) MkUrlSource
  {-# INLINE labelOptic #-}
