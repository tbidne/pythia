-- | Provides the 'Supremum' type.
--
-- @since 0.1
module Pythia.Data.Supremum
  ( Supremum (..),
  )
where

import Pythia.Prelude

-- | Newtype wrapper for easily deriving 'Semigroup' and 'Monoid' instances.
--
-- @since 0.1
newtype Supremum a = MkSupremum a
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord
    )

-- | @since 0.1
instance Ord a => Semigroup (Supremum a) where
  (<>) = max

-- | @since 0.1
instance (Bounded a, Ord a) => Monoid (Supremum a) where
  mempty = minBound
