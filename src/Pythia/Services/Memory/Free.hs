{-# LANGUAGE QuasiQuotes #-}

-- | This module provides functionality for retrieving memory usage using Free.
--
-- @since 0.1
module Pythia.Services.Memory.Free
  ( -- * Query
    memoryShellApp,
    supported,

    -- * Misc
    FreeParseError (..),
    parseMemory,
  )
where

import Data.Bytes (Bytes (MkBytes))
import Data.Char qualified as Char
import Data.Text qualified as T
import Pythia.Control.Exception (fromPythiaException, toPythiaException)
import Pythia.Internal.ShellApp
  ( SimpleShell
      ( MkSimpleShell,
        command,
        isSupported,
        parser
      ),
  )
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.Memory.Types
  ( Memory (MkMemory),
    SystemMemory (MkSystemMemory),
  )
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
      Show
    )

-- | @since 0.1
instance Exception FreeParseError where
  displayException (MkFreeParseError e) =
    ("Could not parse memory from: " <>)
      . T.unpack
      $ e

  toException = toPythiaException
  fromException = fromPythiaException

-- | Free query for 'Memory'.
--
-- @since 0.1
memoryShellApp ::
  ( HasCallStack,
    PathReader :> es,
    TypedProcess :> es
  ) =>
  Eff es SystemMemory
memoryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "free --bytes",
          isSupported = supported,
          parser = parseMemory
        }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: (HasCallStack, PathReader :> es) => Eff es Bool
supported = U.exeSupported [osp|free|]

-- | Attempts to parse the output of free.
--
-- @since 0.1
parseMemory :: Text -> Either FreeParseError SystemMemory
parseMemory txt = case U.foldAlt parseLine ts of
  Nothing -> Left $ MkFreeParseError txt
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
  pure $ MkSystemMemory (MkMemory (MkBytes total)) (MkMemory (MkBytes (used + shared)))
  where
    parseNat = parseBytes readNat
    parsePos = parseBytes readPos
    parseBytes parseFn = do
      MPC.space1
      num <- MP.takeWhile1P Nothing Char.isDigit
      maybe empty pure (parseFn num)
    readNat = TR.readMaybe . T.unpack
    readPos = TR.readMaybe . T.unpack
