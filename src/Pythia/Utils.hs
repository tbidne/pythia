-- | This module provides common utilities.
--
-- @since 0.1.0.0
module Pythia.Utils
  ( headMaybe,
    eitherToBool,
    foldAlt,
    mAlt,
    takeLine,
    takeLineLabel,
    takeLine_,
    exeSupported,
  )
where

import Data.Maybe qualified as May
import Pythia.Prelude
import System.Directory qualified as Dir
import Text.Megaparsec (Parsec, Stream, Token, Tokens)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> :set -XOverloadedStrings

-- | Safe version of 'Prelude.head'.
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

-- | Convenience function for mapping a 'Maybe' to its underlying
-- 'Alternative'.
--
-- @since 0.1.0.0
mAlt :: Alternative f => Maybe (f a) -> f a
mAlt = fromMaybe empty

-- | 'takeLineLabel' with no label.
--
-- @since 0.1.0.0
takeLine :: (Ord e, Stream s, Token s ~ Char) => Parsec e s (Tokens s)
takeLine = takeLineLabel Nothing

-- | Takes everything up to the first new line, returns the result. Also
-- takes in a label.
--
-- @since 0.1.0.0
takeLineLabel :: (Ord e, Stream s, Token s ~ Char) => Maybe String -> Parsec e s (Tokens s)
takeLineLabel desc = MP.takeWhileP desc (/= '\n') <* MPC.eol

-- | Takes everything up to the first new line, returns unit.
--
-- @since 0.1.0.0
takeLine_ :: (Ord e, Stream s, Token s ~ Char) => Parsec e s ()
takeLine_ = MP.takeWhileP Nothing (/= '\n') *> void MPC.eol

-- | Maps 'Left' to 'False, 'Right' to 'True'.
--
-- @since 0.1.0.0
eitherToBool :: Either a b -> Bool
eitherToBool = either (const False) (const True)

-- | Determines if the executable represented by the string parameter is
-- supported on this system.
exeSupported :: String -> IO Bool
exeSupported exeName = May.isJust <$> Dir.findExecutable exeName
