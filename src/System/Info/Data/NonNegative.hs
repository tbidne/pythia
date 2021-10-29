-- | Provides the 'NonNegative' type for safe mathematical
-- operations.
module System.Info.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative, unNonNegative),

    -- * Creation
    mkNonNegative,
    readNonNegative,
    unsafeNonNegative,
  )
where

import Control.Monad ((>=>))
import Text.Read qualified as TR

-- | Newtype wrapper over 'a'.
newtype NonNegative a = MkUnsafeNonNegative
  { -- | Unwraps the 'NonNegative'
    unNonNegative :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonNegative'.
pattern MkNonNegative :: a -> NonNegative a
pattern MkNonNegative n <- MkUnsafeNonNegative n

{-# COMPLETE MkNonNegative #-}

-- | Smart constructor for 'NonNegative'.
--
-- Examples:
--
-- >>> mkNonNegative 7
-- Just (MkNonNegative {getNonNegative = 7})
--
-- >>> mkNonNegative (-2)
-- Nothing
mkNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
mkNonNegative n
  | n >= 0 = Just $ MkUnsafeNonNegative n
  | otherwise = Nothing

-- | Unsafe constructor for 'NonNegative', intended to be used with
-- known constants, e.g., @unsafeNonNegative 7@. Exercise restraint!
unsafeNonNegative :: (Num a, Ord a, Show a) => a -> NonNegative a
unsafeNonNegative n
  | n >= 0 = MkUnsafeNonNegative n
  | otherwise =
    error $
      "Passed negative "
        <> show n
        <> " to unsafeNonNegative!"

-- | Safely attempts to read a 'NonNegative'.
--
-- >>> readNonNegative "5"
-- Just (MkUnsafeNonNegative {unNonNegative = 5})
--
-- >>> readNonNegative "cat"
-- Nothing
--
-- >>> readNonNegative "-5"
-- Nothing
readNonNegative :: (Num a, Ord a, Read a) => String -> Maybe (NonNegative a)
readNonNegative = TR.readMaybe >=> mkNonNegative
