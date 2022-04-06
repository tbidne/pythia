-- | Provides the 'Supremum' type.
--
-- @since 0.1.0.0
module Pythia.Data.Supremum
  ( Supremum (..),
  )
where

import Pythia.Prelude

-- | Newtype wrapper for easily deriving 'Semigroup' and 'Monoid' instances.
--
-- @since 0.1.0.0
newtype Supremum a = MkSupremum a
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord
    )

-- | @since 0.1.0.0
instance Ord a => Semigroup (Supremum a) where
  (<>) = max

-- | @since 0.1.0.0
instance (Bounded a, Ord a) => Monoid (Supremum a) where
  mempty = minBound
