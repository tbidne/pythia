{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Export LocalIps types.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Types
  ( Ipv4 (..),
    Ipv6 (..),
  )
where

import Data.Text qualified as T
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Refined (Refined, SizeLessThan, type (&&), type (||))
import Refined qualified as R
import Refined.Extras.Predicates.Foldable (All)
import Refined.Extras.Predicates.Text (Digit, HexDigit, SymEqualTo)

-- | Type for an Ipv4 address, i.e., a string of max length 15 with a mix of
-- digits and dots.
--
-- @since 0.1.0.0
newtype Ipv4 = MkIpv4
  { unIpv4 :: Refined (All (Digit || SymEqualTo ".") && SizeLessThan 16) Text
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter Ipv4 where
  pretty = T.unpack . R.unrefine . unIpv4

makeFieldLabelsNoPrefix ''Ipv4

-- | Type for an Ipv6 address, i.e., a string of max length 39 with a mix of
-- hex digits and colons.
--
-- @since 0.1.0.0
newtype Ipv6 = MkIpv6
  { unIpv6 :: Refined (All (HexDigit || SymEqualTo ":") && SizeLessThan 40) Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter Ipv6 where
  pretty = T.unpack . R.unrefine . unIpv6

makeFieldLabelsNoPrefix ''Ipv6
