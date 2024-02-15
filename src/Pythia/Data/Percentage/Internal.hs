{-# LANGUAGE ViewPatterns #-}

-- | Internal module for 'Percentage'.
--
-- @since 0.1
module Pythia.Data.Percentage.Internal
  ( Percentage (.., MkPercentage),
    unPercentage,
  )
where

import Language.Haskell.TH.Syntax (Lift)
import Numeric.Data.Interval (Interval (MkInterval), IntervalBound (Closed))
import Pythia.Prelude

-- | Represents a percentage.
--
-- @since 0.1
type Percentage :: Type
newtype Percentage = InternalPercentage (Interval (Closed 0) (Closed 100) Word8)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | Pattern synonym for Percentage.
--
-- @since 0.1
pattern MkPercentage :: Word8 -> Percentage
pattern MkPercentage x <- (unPercentage -> x)

{-# COMPLETE MkPercentage #-}

-- | @since 0.1
instance Display Percentage where
  displayBuilder (InternalPercentage (MkInterval x)) =
    displayBuilder (show x) <> "%"

-- | Retrieve the raw percentage.
--
-- @since 0.1
unPercentage :: Percentage -> Word8
unPercentage (InternalPercentage (MkInterval x)) = x
