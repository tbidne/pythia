-- | This module provides common utilities.
--
-- @since 0.1.0.0
module Pythia.Utils
  ( -- * Folding
    foldAlt,
    mAlt,

    -- * Parsing
    takeLine,
    takeLineLabel,
    takeLine_,
    exeSupported,

    -- * Miscellaneous
    headMaybe,
    eitherToBool,
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
-- >>> import Text.Megaparsec (parseTest)

-- | Safe version of 'Prelude.head'.
--
-- @since 0.1.0.0
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Similar to 'foldMap' but for 'Alternative'.
--
-- ==== __Examples__
-- >>> foldAlt (\c -> if even c then Just c else Nothing) [1,2,3,4]
-- Just 2
--
-- >>> foldAlt (\c -> if even c then Just c else Nothing) [1,3]
-- Nothing
--
-- @since 0.1.0.0
foldAlt :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldAlt f = foldr ((<|>) . f) empty

-- | Convenience function for mapping a 'Maybe' to its underlying
-- 'Alternative'.
--
-- ==== __Examples__
-- >>> mAlt @[] Nothing
-- []
--
-- >>> mAlt @[] (Just [1,2,3])
-- [1,2,3]
--
-- @since 0.1.0.0
mAlt :: Alternative f => Maybe (f a) -> f a
mAlt = fromMaybe empty

-- | 'takeLineLabel' with no label.
--
-- ==== __Examples__
-- >>> parseTest @Void takeLine "some text 123 \n"
-- "some text 123 "
--
-- >>> parseTest @Void takeLine "some text 123"
-- 1:14:
--   |
-- 1 | some text 123
--   |              ^
-- unexpected end of input
-- expecting end of line
--
-- @since 0.1.0.0
takeLine :: (Ord e, Stream s, Token s ~ Char) => Parsec e s (Tokens s)
takeLine = takeLineLabel Nothing

-- | Variant of 'takeLine' taking in a label.
--
-- ==== __Examples__
-- >>> parseTest @Void (takeLineLabel (Just "a label")) "some text 123"
-- 1:14:
--   |
-- 1 | some text 123
--   |              ^
-- unexpected end of input
-- expecting a label or end of line
--
-- @since 0.1.0.0
takeLineLabel :: (Ord e, Stream s, Token s ~ Char) => Maybe String -> Parsec e s (Tokens s)
takeLineLabel desc = MP.takeWhileP desc (/= '\n') <* MPC.eol

-- | Takes everything up to the first new line, returns unit.
--
-- ==== __Examples__
-- >>> parseTest @Void takeLine_ "some text 123\n"
-- ()
--
-- @since 0.1.0.0
takeLine_ :: (Ord e, Stream s, Token s ~ Char) => Parsec e s ()
takeLine_ = MP.takeWhileP Nothing (/= '\n') *> void MPC.eol

-- | Maps 'Left' to 'False', 'Right' to 'True'.
--
-- ==== __Examples__
-- >>> eitherToBool (Left ())
-- False
--
-- >>> eitherToBool (Right ())
-- True
--
-- @since 0.1.0.0
eitherToBool :: Either a b -> Bool
eitherToBool = either (const False) (const True)

-- | Determines if the executable represented by the string parameter is
-- supported on this system.
--
-- @since 0.1.0.0
exeSupported :: MonadIO m => String -> m Bool
exeSupported exeName = liftIO $ May.isJust <$> Dir.findExecutable exeName
