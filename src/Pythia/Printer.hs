{-# LANGUAGE DefaultSignatures #-}

-- | This modules provides functionality for pretty printing query results.
--
-- @since 0.1.0.0
module Pythia.Printer
  ( PrettyPrinter (..),
    joinCommas,
    joinNewlines,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Pythia.Prelude

-- | Typeclass for pretty printing.
--
-- @since 0.1.0.0
class PrettyPrinter a where
  -- | @since 0.1.0.0
  pretty :: a -> String
  default pretty :: Show a => a -> String
  pretty = show

-- | @since 0.1.0.0
instance PrettyPrinter String where
  pretty = id

-- | @since 0.1.0.0
instance PrettyPrinter Text where
  pretty = T.unpack

-- | @since 0.1.0.0
instance PrettyPrinter a => PrettyPrinter (Maybe a) where
  pretty Nothing = ""
  pretty (Just x) = pretty x

-- | Join with commas.
--
-- @since 0.1.0.0
joinCommas :: PrettyPrinter a => [a] -> String
joinCommas = L.intercalate ", " . fmap pretty

-- | Join with newlines.
--
-- @since 0.1.0.0
joinNewlines :: PrettyPrinter a => [a] -> String
joinNewlines = L.intercalate "\n" . fmap pretty
