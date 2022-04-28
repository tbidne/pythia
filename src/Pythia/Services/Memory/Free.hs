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

import ByteTypes.Bytes (Bytes (..))
import Data.Char qualified as Char
import Data.Text qualified as T
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Prelude
import Pythia.Services.Memory.Types (Memory (..))
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
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
instance PrettyPrinter FreeException where
  pretty (FreeGeneralException e) = "Free exception: <" <> T.pack (displayException e) <> ">"
  pretty (FreeParseException s) = "Free parse exception: <" <> s <> ">"

-- | @since 0.1
instance Exception FreeException where
  displayException = T.unpack . pretty
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
memoryShellApp :: MonadUnliftIO m => m Memory
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
parseMemory :: Text -> Either FreeException Memory
parseMemory txt = case U.foldAlt parseLine ts of
  Nothing -> Left $ FreeParseException $ "Could not parse memory input: " <> txt
  Just mem -> Right mem
  where
    ts = T.lines txt

parseLine :: Text -> Maybe Memory
parseLine ln = case MP.parse mparseMemory "Memory.hs" ln of
  Right mem -> Just mem
  Left _ -> Nothing

type MParser = Parsec Void Text

mparseMemory :: MParser Memory
mparseMemory = do
  MPC.string' "Mem:"
  total <- parseBytes
  used <- parseBytes
  parseBytes
  shared <- parseBytes
  pure $ MkMemory (MkBytes total) (MkBytes (used + shared))
  where
    parseBytes = do
      MPC.space1
      num <- MP.takeWhile1P Nothing Char.isDigit
      maybe empty pure (readNat num)
    readNat = TR.readMaybe . T.unpack
