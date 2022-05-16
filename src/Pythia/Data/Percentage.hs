{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Percentage' type.
--
-- @since 0.1
module Pythia.Data.Percentage
  ( Percentage (..),
    rawPercentage,
  )
where

import Numeric.Data.Interval (LRInterval (..))
import Pythia.Prelude
import Pythia.Utils (Pretty (..))

-- | Represents a percentage.
--
-- @since 0.1
type Percentage :: Type
newtype Percentage = MkPercentage {unPercentage :: LRInterval 0 100 Word8}
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
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Percentage

-- | @since 0.1
instance Pretty Percentage where
  pretty (MkPercentage p) = pretty p <> pretty @Text "%"
  {-# INLINEABLE pretty #-}

-- | Retrieve the raw percentage.
--
-- @since 0.1
rawPercentage :: Percentage -> Word8
rawPercentage (MkPercentage (MkLRInterval x)) = x
{-# INLINEABLE rawPercentage #-}
