{-# LANGUAGE CPP #-}

-- | This module provides common utilities.
--
-- @since 0.1
module Pythia.Utils
  ( -- * Folding
    foldAlt,
    mAlt,

    -- * Parsing
    takeLine,
    takeLineLabel,
    takeLine_,
    exeSupported,

    -- * Pretty Printing
    Pretty (..),
    Pretty.Doc,
    (<+>),
    Pretty.comma,
    Pretty.punctuate,
    Pretty.hsep,
    Pretty.vsep,
    Pretty.layoutCompact,
    PrettyText.renderStrict,
    prettyToText,

    -- * Miscellaneous
    headMaybe,
    eitherToBool,
  )
where

import Data.Maybe qualified as May
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
import Data.Text.Prettyprint.Doc qualified as Pretty
import Data.Text.Prettyprint.Doc.Render.Text qualified as PrettyText
#else
import Prettyprinter (Pretty (..), (<+>))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as PrettyText
#endif
import Pythia.Prelude
import System.Directory qualified as Dir
import Text.Megaparsec (Parsec, Stream, Token, Tokens)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Megaparsec (parseTest)

-- | Similar to 'foldMap' but for 'Alternative'.
--
-- ==== __Examples__
--
-- >>> foldAlt (\c -> if even c then Just c else Nothing) [1,2,3,4]
-- Just 2
--
-- >>> foldAlt (\c -> if even c then Just c else Nothing) [1,3]
-- Nothing
--
-- @since 0.1
foldAlt :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldAlt f = foldr ((<|>) . f) empty

-- | Convenience function for mapping a 'Maybe' to its underlying
-- 'Alternative'.
--
-- ==== __Examples__
--
-- >>> mAlt @[] Nothing
-- []
--
-- >>> mAlt @[] (Just [1,2,3])
-- [1,2,3]
--
-- @since 0.1
mAlt :: Alternative f => Maybe (f a) -> f a
mAlt = fromMaybe empty

-- | 'takeLineLabel' with no label.
--
-- ==== __Examples__
--
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
-- @since 0.1
takeLine :: (Ord e, Stream s, Token s ~ Char) => Parsec e s (Tokens s)
takeLine = takeLineLabel Nothing

-- | Variant of 'takeLine' taking in a label.
--
-- ==== __Examples__
--
-- >>> parseTest @Void (takeLineLabel (Just "a label")) "some text 123"
-- 1:14:
--   |
-- 1 | some text 123
--   |              ^
-- unexpected end of input
-- expecting a label or end of line
--
-- @since 0.1
takeLineLabel :: (Ord e, Stream s, Token s ~ Char) => Maybe String -> Parsec e s (Tokens s)
takeLineLabel desc = MP.takeWhileP desc (/= '\n') <* MPC.eol

-- | Takes everything up to the first new line, returns unit.
--
-- ==== __Examples__
--
-- >>> parseTest @Void takeLine_ "some text 123\n"
-- ()
--
-- @since 0.1
takeLine_ :: (Ord e, Stream s, Token s ~ Char) => Parsec e s ()
takeLine_ = MP.takeWhileP Nothing (/= '\n') *> void MPC.eol

-- | Maps 'Left' to 'False', 'Right' to 'True'.
--
-- ==== __Examples__
--
-- >>> eitherToBool (Left ())
-- False
--
-- >>> eitherToBool (Right ())
-- True
--
-- @since 0.1
eitherToBool :: Either a b -> Bool
eitherToBool = either (const False) (const True)

-- | Determines if the executable represented by the string parameter is
-- supported on this system.
--
-- @since 0.1
exeSupported :: MonadIO m => String -> m Bool
exeSupported exeName = liftIO $ May.isJust <$> Dir.findExecutable exeName

-- | Converts a type with a 'Pretty' instance to 'Text'.
--
-- @since 0.1
prettyToText :: Pretty a => a -> Text
prettyToText = PrettyText.renderStrict . Pretty.layoutCompact . pretty
