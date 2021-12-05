-- | This module provides common utilities.
--
-- @since 0.1.0.0
module Pythia.Utils
  ( headMaybe,
    foldAlt,
    takeLine1,
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Functor (($>))

-- $setup
-- >>> :set -XOverloadedStrings

-- | Safe version of 'head'.
--
-- @since 0.1.0.0
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Similar to 'foldMap' but for 'Alternative'.
--
-- @since 0.1.0.0
foldAlt :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldAlt f = foldr ((<|>) . f) empty

-- | Takes the rest of the line, including 1 or more new line characters.
--
-- ==== __Examples__
-- >>> AP.parseOnly takeLine1 "abc edf \n\n\n"
-- Right ()
--
-- >>> AP.parseOnly takeLine1 "\n"
-- Right ()
--
-- >>> AP.parseOnly takeLine1 "abc"
-- Left "not enough input"
--
-- >>> AP.parseOnly takeLine1 ""
-- Left "not enough input"
--
-- @since 0.1.0.0
takeLine1 :: Parser ()
takeLine1 =
  AP.takeWhile (not . AP.isEndOfLine)
    *> AP.many1 AP.endOfLine
    $> ()
