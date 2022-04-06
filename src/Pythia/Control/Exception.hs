{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides various exception types.
--
-- @since 0.1.0.0
module Pythia.Control.Exception
  ( -- * Exception Hierarchy Root
    PythiaException (..),
    toExceptionViaPythia,
    fromExceptionViaPythia,

    -- * Miscellaneous Exceptions
    CommandException (..),
    MultiExceptions (..),
    NotSupportedException (..),
    NoActionsRunException (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Typeable (cast)
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Class.Printer qualified as Printer
import Pythia.Data.Command (Command (..))
import Pythia.Prelude

-- | All specific exceptions thrown by pythia are subtypes of
-- 'PythiaException'.
--
-- @since 0.1.0.0
data PythiaException = forall e. Exception e => MkPythiaException e
  deriving anyclass
    ( -- | @since 0.1.0.0
      PrettyPrinter
    )

-- | @since 0.1.0.0
deriving stock instance Show PythiaException

-- | @since 0.1.0.0
instance Exception PythiaException where
  displayException (MkPythiaException e) = displayException e

-- | 'toException' via 'PythiaException'. Used for defining an exception

--- as a subtype of 'PythiaException'.
--
-- @since 0.1.0.0
toExceptionViaPythia :: Exception e => e -> SomeException
toExceptionViaPythia = toException . MkPythiaException

-- | 'fromException' via 'PythiaException'. Used for defining an exception

--- as a subtype of 'PythiaException'.
--
-- @since 0.1.0.0
fromExceptionViaPythia :: Exception e => SomeException -> Maybe e
fromExceptionViaPythia x = do
  MkPythiaException e <- fromException x
  cast e

-- | Exceptions encountered while running a shell command.
--
-- @since 0.1.0.0
data CommandException = MkCommandException
  { -- | The command that was run.
    --
    -- @since 0.1.0.0
    command :: Command,
    -- | The error message received.
    --
    -- @since 0.1.0.0
    errMsg :: String
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''CommandException

-- | @since 0.1.0.0
instance Exception CommandException where
  displayException = pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | @since 0.1.0.0
instance PrettyPrinter CommandException where
  pretty (MkCommandException (MkCommand c) s) =
    "Command exception. Command: <" <> T.unpack c <> ">"
      <> ". Error: <"
      <> s
      <> ">"

-- | Collects multiple exceptions.
--
-- @since 0.1.0.0
newtype MultiExceptions = MkMultiExceptions
  { -- | @since 0.1.0.0
    unExceptions :: NonEmpty SomeException
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''MultiExceptions

-- | @since 0.1.0.0
instance Exception MultiExceptions where
  displayException = pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | @since 0.1.0.0
instance PrettyPrinter MultiExceptions where
  pretty (MkMultiExceptions xs) =
    "Multiple exceptions: " <> delim
      <> Printer.joinX delim (NE.toList $ fmap displayException xs)
    where
      delim = "\n  - "

-- | @since 0.1.0.0
instance Semigroup MultiExceptions where
  MkMultiExceptions l <> MkMultiExceptions r = MkMultiExceptions $ l <> r

-- | Error for when the current app is not supported.
--
-- @since 0.1.0.0
newtype NotSupportedException = MkNotSupportedException
  { -- | @since 0.1.0.0
    unNotSupportedException :: String
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''NotSupportedException

-- | @since 0.1.0.0
instance Exception NotSupportedException where
  displayException = pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | @since 0.1.0.0
instance PrettyPrinter NotSupportedException where
  pretty (MkNotSupportedException s) = "App not supported: <" <> s <> ">"

-- | Error for when the current app is not supported.
--
-- @since 0.1.0.0
data NoActionsRunException = MkNoActionsRunException
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance PrettyPrinter NoActionsRunException where
  pretty MkNoActionsRunException = "No actions run"

-- | @since 0.1.0.0
instance Exception NoActionsRunException where
  displayException = pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia
