{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Export LocalIps types.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Types
  ( -- * IP Types
    Ipv4 (..),
    Ipv6 (..),

    -- * Refined Re-exports
    Refined,
    type (\/),
    All,
    Digit,
    HexDigit,
    MaxLength,
    SymEquals,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Optics.TH qualified as OTH
import Pythia.Printer (PrettyPrinter (..))
import Refined
  ( All,
    Digit,
    HexDigit,
    MaxLength,
    Refined (..),
    SymEquals,
    type (\/),
  )

-- | Type for an Ipv4 address, i.e., a string of max length 15 with a mix of
-- digits and dots.
--
-- @since 0.1.0.0
newtype Ipv4 = MkIpv4
  { unIpv4 :: Refined [All (Digit \/ SymEquals "."), MaxLength 15] Text
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter Ipv4 where
  pretty = T.unpack . unrefine . unIpv4

OTH.makeFieldLabelsNoPrefix ''Ipv4

-- | Type for an Ipv6 address, i.e., a string of max length 39 with a mix of
-- hex digits and colons.
--
-- @since 0.1.0.0
newtype Ipv6 = MkIpv6
  { unIpv6 :: Refined [All (HexDigit \/ SymEquals ":"), MaxLength 39] Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter Ipv6 where
  pretty = T.unpack . unrefine . unIpv6

OTH.makeFieldLabelsNoPrefix ''Ipv6
