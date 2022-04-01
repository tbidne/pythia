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

import Pythia.Prelude
import System.Directory qualified as Dir
import Text.Megaparsec (Parsec, Stream, Token, Tokens)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

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

mAlt :: Alternative f => Maybe (f a) -> f a
mAlt = fromMaybe empty

takeLine :: (Ord e, Stream s, Token s ~ Char) => Parsec e s (Tokens s)
takeLine = takeLineLabel Nothing

takeLineLabel :: (Ord e, Stream s, Token s ~ Char) => Maybe String -> Parsec e s (Tokens s)
takeLineLabel desc = MP.takeWhileP desc (/= '\n') <* MPC.eol

takeLine_ :: (Ord e, Stream s, Token s ~ Char) => Parsec e s ()
takeLine_ = MP.takeWhileP Nothing (/= '\n') *> void MPC.eol

-- | Maps 'Left' to 'False, 'Right' to 'True'.
--
-- @since 0.1.0.0
eitherToBool :: Either a b -> Bool
eitherToBool = either (const False) (const True)

exeSupported :: String -> IO Bool
exeSupported exeName = maybe False (const True) <$> Dir.findExecutable exeName
