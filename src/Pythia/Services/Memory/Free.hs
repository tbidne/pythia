{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functionality for retrieving memory usage using Free.
--
-- @since 0.1
module Pythia.Services.Memory.Free
  ( -- * Query
    memoryShellApp,
    supported,

    -- * Misc
    FreeParseError (..),
    _MkFreeParseError,
    parseMemory,
  )
where

import Data.Bytes (Bytes (..))
import Data.Char qualified as Char
import Data.Text qualified as T
import Numeric.Algebra (ASemigroup (..))
import Numeric.Data.NonNegative qualified as NN
import Numeric.Data.Positive qualified as Pos
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Internal.ShellApp (SimpleShell (..))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.Memory.Types (Memory (..), SystemMemory (..))
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

-- | Errors that can occur with acpi.
--
-- ==== __Examples__
--
-- >>> displayException $ MkFreeParseError "parse error"
-- "Could not parse memory from: parse error"
--
-- @since 0.1
type FreeParseError :: Type
newtype FreeParseError = MkFreeParseError Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''FreeParseError

-- | @since 0.1
instance Pretty FreeParseError where
  pretty (MkFreeParseError s) =
    pretty @Text "Could not parse memory from: " <> pretty s
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception FreeParseError where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | Free query for 'Memory'.
--
-- __Throws:__
--
-- * 'FreeException': if something goes wrong (i.e. exception while running
--       the command, or we have a parse error).
--
-- @since 0.1
memoryShellApp :: IO SystemMemory
memoryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "free --bytes",
          parser = parseMemory
        }
{-# INLINEABLE memoryShellApp #-}

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: IO Bool
supported = U.exeSupported "free"
{-# INLINEABLE supported #-}

-- | Attempts to parse the output of free.
--
-- @since 0.1
parseMemory :: Text -> Either FreeParseError SystemMemory
parseMemory txt = case U.foldAlt parseLine ts of
  Nothing -> Left $ MkFreeParseError txt
  Just mem -> Right mem
  where
    ts = T.lines txt
{-# INLINEABLE parseMemory #-}

parseLine :: Text -> Maybe SystemMemory
parseLine ln = case MP.parse mparseMemory "Memory.hs" ln of
  Right mem -> Just mem
  Left _ -> Nothing
{-# INLINEABLE parseLine #-}

type MParser :: Type -> Type
type MParser = Parsec Void Text

mparseMemory :: MParser SystemMemory
mparseMemory = do
  MPC.string' "Mem:"
  total <- parsePos
  used <- parseNat
  parseNat
  shared <- parseNat
  pure $ MkSystemMemory (MkMemory (MkBytes total)) (MkMemory (MkBytes (used .+. shared)))
  where
    parseNat = parseBytes readNat
    parsePos = parseBytes readPos
    parseBytes parseFn = do
      MPC.space1
      num <- MP.takeWhile1P Nothing Char.isDigit
      maybe empty pure (parseFn num)
    readNat = (NN.mkNonNegative <=< TR.readMaybe) . T.unpack
    readPos = (Pos.mkPositive <=< TR.readMaybe) . T.unpack
{-# INLINEABLE mparseMemory #-}
