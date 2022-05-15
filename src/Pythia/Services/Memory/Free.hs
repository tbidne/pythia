{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving memory usage using Free.
--
-- @since 0.1
module Pythia.Services.Memory.Free
  ( -- * Query
    memoryShellApp,
    supported,

    -- * Misc
    FreeException (..),
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
import Pythia.Prelude
import Pythia.Services.Memory.Types (Memory (..), SystemMemory (..))
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- $setup
-- >>> import GHC.Exception (errorCallException)

-- | Errors that can occur with acpi.
--
-- ==== __Examples__
--
-- >>> putStrLn $ displayException $ FreeGeneralException $ errorCallException "oh no"
-- Free exception: <oh no>
--
-- >>> putStrLn $ displayException $ FreeParseException "parse error"
-- Free parse exception: <parse error>
--
-- @since 0.1
type FreeException :: Type
data FreeException
  = -- | For general exceptions.
    --
    -- @since 0.1
    forall e. Exception e => FreeGeneralException e
  | -- | Parse errors.
    --
    -- @since 0.1
    FreeParseException Text

-- | @since 0.1
makePrismLabels ''FreeException

-- | @since 0.1
deriving stock instance Show FreeException

-- | @since 0.1
instance Pretty FreeException where
  pretty (FreeGeneralException e) =
    pretty @Text "Free exception: <"
      <> pretty (displayException e)
      <> ">"
  pretty (FreeParseException s) =
    pretty @Text "Free parse exception: <"
      <> pretty s
      <> pretty @Text ">"

-- | @since 0.1
instance Exception FreeException where
  displayException = T.unpack . U.prettyToText
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | Free query for 'Memory'.
--
-- __Throws:__
--
-- * 'FreeException': if something goes wrong (i.e. exception while running
--       the command, or we have a parse error).
--
-- @since 0.1
memoryShellApp :: (MonadCatch m, MonadIO m) => m SystemMemory
memoryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "free --bytes",
          parser = parseMemory,
          liftShellEx = FreeGeneralException
        }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: MonadIO m => m Bool
supported = U.exeSupported "free"

-- | Attempts to parse the output of free.
--
-- @since 0.1
parseMemory :: Text -> Either FreeException SystemMemory
parseMemory txt = case U.foldAlt parseLine ts of
  Nothing -> Left $ FreeParseException $ "Could not parse memory input: " <> txt
  Just mem -> Right mem
  where
    ts = T.lines txt

parseLine :: Text -> Maybe SystemMemory
parseLine ln = case MP.parse mparseMemory "Memory.hs" ln of
  Right mem -> Just mem
  Left _ -> Nothing

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
