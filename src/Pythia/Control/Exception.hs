{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides various exception types.
--
-- @since 0.1.0.0
module Pythia.Control.Exception
  ( -- * Primary Types
    CommandException (..),
    MultiExceptions (..),
    NotSupportedException (..),
    NoActionsRunException (..),

    -- * Utilities
    PrettyException (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Pythia.Data.Command (Command (..))
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Printer qualified as Printer

-- | This type exists solely to derive exceptions based on a pretty instance.
-- It is not intended to be thrown anywhere.
--
-- @since 0.1.0.0
newtype PrettyException a = MkPrettyException
  { -- | @since 0.1.0.0
    unPrettyException :: a
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance (Show e, PrettyPrinter e, Typeable e) => Exception (PrettyException e) where
  displayException = pretty . unPrettyException

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
deriving via (PrettyException CommandException) instance Exception CommandException

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
deriving via (PrettyException MultiExceptions) instance Exception MultiExceptions

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
deriving via (PrettyException NotSupportedException) instance Exception NotSupportedException

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
  deriving
    ( -- | @since 0.1.0.0
      Exception
    )
    via (PrettyException NoActionsRunException)

-- | @since 0.1.0.0
instance PrettyPrinter NoActionsRunException where
  pretty MkNoActionsRunException = "No actions run"
