{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides common network types.
--
-- @since 0.1.0.0
module Pythia.Services.Types
  ( Device (..),
    Ipv4Address (..),
    unsafeIpv4Address,
    Ipv6Address (..),
    unsafeIpv6Address,
    IpType (..),
  )
where

import Data.Text qualified as T
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Refined (NonEmpty, Refined, SizeLessThan, type (&&), type (||))
import Refined qualified as R
import Refined.Extras.Predicates.Foldable (All)
import Refined.Extras.Predicates.Text (Digit, HexDigit, SymEqualTo)
import Refined.Unsafe qualified as R

-- | Newtype wrapper over a network device name.
--
-- @since 0.1.0.0
newtype Device = MkDevice
  { -- | @since 0.1.0.0
    unDevice :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Read,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      IsString,
      -- | @since 0.1.0.0
      PrettyPrinter
    )
    via Text

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''Device

-- | Type for an Ipv4Address address, i.e., a string of max length 15 with a mix of
-- digits and dots.
--
-- @since 0.1.0.0
newtype Ipv4Address = MkIpv4Address
  { unIpv4 :: Refined (All (Digit || SymEqualTo ".") && SizeLessThan 16 && NonEmpty) Text
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter Ipv4Address where
  pretty = T.unpack . R.unrefine . unIpv4

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''Ipv4Address

-- | @since 0.1.0.0
unsafeIpv4Address :: Text -> Ipv4Address
unsafeIpv4Address = MkIpv4Address . R.unsafeRefine

-- | Type for an Ipv6Address address, i.e., a string of max length 39 with a mix of
-- hex digits and colons.
--
-- @since 0.1.0.0
newtype Ipv6Address = MkIpv6Address
  { unIpv6 :: Refined (All (HexDigit || SymEqualTo ":") && SizeLessThan 40 && NonEmpty) Text
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
instance PrettyPrinter Ipv6Address where
  pretty = T.unpack . R.unrefine . unIpv6

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''Ipv6Address

-- | @since 0.1.0.0
unsafeIpv6Address :: Text -> Ipv6Address
unsafeIpv6Address = MkIpv6Address . R.unsafeRefine

-- | IP types.
--
-- @since 0.1.0.0
data IpType
  = -- | @since 0.1.0.0
    Ipv4
  | -- | @since 0.1.0.0
    Ipv6
  deriving (Eq, Show)
