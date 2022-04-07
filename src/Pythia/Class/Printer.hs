{-# LANGUAGE DefaultSignatures #-}

-- | This modules provides functionality for pretty printing query results.
--
-- @since 0.1
module Pythia.Class.Printer
  ( PrettyPrinter (..),
    joinCommas,
    joinNewlines,
    joinX,
  )
where

import Data.Text qualified as T
import Pythia.Prelude

-- | Typeclass for pretty printing.
--
-- @since 0.1
class PrettyPrinter a where
  -- | @since 0.1
  pretty :: a -> Text
  default pretty :: Show a => a -> Text
  pretty = T.pack . show

-- | @since 0.1
instance PrettyPrinter String where
  pretty = T.pack

-- | @since 0.1
instance PrettyPrinter Text where
  pretty = id

-- | @since 0.1
instance PrettyPrinter a => PrettyPrinter (Maybe a) where
  pretty Nothing = ""
  pretty (Just x) = pretty x

-- | Join with commas.
--
-- ==== __Examples__
--
-- >>> joinCommas ["foo", "bar"]
-- "foo, bar"
--
-- @since 0.1
joinCommas :: PrettyPrinter a => [a] -> Text
joinCommas = joinX ", "

-- | Join with newlines.
--
-- ==== __Examples__
--
-- >>> joinNewlines ["foo", "bar"]
-- "foo\nbar"
--
-- @since 0.1
joinNewlines :: PrettyPrinter a => [a] -> Text
joinNewlines = joinX "\n"

-- | General list join.
--
-- ==== __Examples__
--
-- >>> joinX "--" ["foo", "bar"]
-- "foo--bar"
--
-- @since 0.1
joinX :: PrettyPrinter a => Text -> [a] -> Text
joinX s = T.intercalate s . fmap pretty
