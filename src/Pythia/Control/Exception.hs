{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides various exception types.
--
-- @since 0.1
module Pythia.Control.Exception
  ( -- * Exception Hierarchy Root
    PythiaException (..),
    toExceptionViaPythia,
    fromExceptionViaPythia,

    -- * Miscellaneous Exceptions
    CommandException (..),
    SomeExceptions (..),
    someExceptionsIso,
    NotSupportedException (..),
    notSupportedExceptionIso,
    NoActionsRunException (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Typeable (cast)
import Pythia.Data.Command (Command (..))
import Pythia.Prelude
import Pythia.Utils (Pretty (..), (<+>))
import Pythia.Utils qualified as U

-- | All specific exceptions thrown by pythia are subtypes of
-- 'PythiaException'.
--
-- @since 0.1
type PythiaException :: Type
data PythiaException = forall e. Exception e => MkPythiaException e

-- | @since 0.1
deriving stock instance Show PythiaException

-- | @since 0.1
instance Exception PythiaException where
  displayException (MkPythiaException e) = displayException e
  {-# INLINEABLE displayException #-}

-- | @since 0.1
instance Pretty PythiaException where
  pretty = pretty . displayException
  {-# INLINEABLE pretty #-}

-- | 'toException' via 'PythiaException'. Used for defining an exception
-- as a subtype of 'PythiaException'.
--
-- @since 0.1
toExceptionViaPythia :: Exception e => e -> SomeException
toExceptionViaPythia = toException . MkPythiaException
{-# INLINEABLE toExceptionViaPythia #-}

-- | 'fromException' via 'PythiaException'. Used for defining an exception
-- as a subtype of 'PythiaException'.
--
-- @since 0.1
fromExceptionViaPythia :: Exception e => SomeException -> Maybe e
fromExceptionViaPythia x = do
  MkPythiaException e <- fromException x
  cast e
{-# INLINEABLE fromExceptionViaPythia #-}

-- | Exceptions encountered while running a shell command.
--
-- ==== __Examples__
-- >>> putStrLn $ displayException $ MkCommandException "some command" "an error message"
-- Command exception. Command: <some command>. Error: <an error message>
--
-- @since 0.1
type CommandException :: Type
data CommandException = MkCommandException
  { -- | The command that was run.
    --
    -- @since 0.1
    command :: Command,
    -- | The error message received.
    --
    -- @since 0.1
    errMsg :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''CommandException

-- | @since 0.1
instance Exception CommandException where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | @since 0.1
instance Pretty CommandException where
  pretty (MkCommandException (MkCommand c) s) =
    pretty @String "Command exception. Command: <"
      <> pretty c
      <> pretty @String ">. Error: <"
      <> pretty s
      <> pretty @String ">"
  {-# INLINEABLE pretty #-}

-- | Collects 1 or more exceptions.
--
-- ==== __Examples__
-- >>> let ex1 = toException $ MkCommandException "some command" "an error message"
-- >>> let ex2 = toException $ MkNotSupportedException "app1"
-- >>> putStrLn $ displayException $ MkSomeExceptions $ ex1 :| [ex2]
-- Found 2 exception(s)
-- - Command exception. Command: <some command>. Error: <an error message>
-- - App not supported: <app1>
--
-- @since 0.1
type SomeExceptions :: Type
newtype SomeExceptions = MkSomeExceptions
  { -- | @since 0.1
    unSomeExceptions :: NonEmpty SomeException
  }
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
someExceptionsIso :: Iso' SomeExceptions (NonEmpty SomeException)
someExceptionsIso = iso unSomeExceptions MkSomeExceptions

-- | @since 0.1
instance Exception SomeExceptions where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | @since 0.1
instance Pretty SomeExceptions where
  pretty (MkSomeExceptions xs) = U.vsep (header : exes)
    where
      header = pretty @Text "Found" <+> pretty (showt $ length xs) <+> pretty @Text "exception(s)"
      exes = NE.toList $ fmap ((pretty @Text "-" <+>) . pretty . displayException) xs
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Semigroup SomeExceptions where
  MkSomeExceptions l <> MkSomeExceptions r = MkSomeExceptions $ l <> r
  {-# INLINEABLE (<>) #-}

-- | Error for when the current app is not supported.
--
-- ==== __Examples__
-- >>> putStrLn $ displayException $ MkNotSupportedException "app1"
-- App not supported: <app1>
--
-- @since 0.1
type NotSupportedException :: Type
newtype NotSupportedException = MkNotSupportedException
  { -- | @since 0.1
    unNotSupportedException :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
notSupportedExceptionIso :: Iso' NotSupportedException Text
notSupportedExceptionIso = iso unNotSupportedException MkNotSupportedException

-- | @since 0.1
instance Exception NotSupportedException where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | @since 0.1
instance Pretty NotSupportedException where
  pretty (MkNotSupportedException s) =
    pretty @Text "App not supported: <"
      <> pretty @Text s
      <> pretty @Text ">"
  {-# INLINEABLE pretty #-}

-- | Error for when no actions are run.
--
-- ==== __Examples__
-- >>> putStrLn $ displayException MkNoActionsRunException
-- No actions run
--
-- @since 0.1
type NoActionsRunException :: Type
data NoActionsRunException = MkNoActionsRunException
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Pretty NoActionsRunException where
  pretty MkNoActionsRunException = "No actions run"
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception NoActionsRunException where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}
