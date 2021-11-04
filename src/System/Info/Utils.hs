-- | This module provides common utilities.
module System.Info.Utils
  ( headMaybe,
    foldAlt,
  )
where

import Control.Applicative (Alternative (..))

-- | Safe version of 'head'.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Similar to 'foldMap' but for 'Alternative'.
foldAlt :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldAlt f = foldr ((<|>) . f) empty
