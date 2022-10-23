{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides various exception types.
--
-- @since 0.1
module Pythia.Control.Exception
  ( -- * Miscellaneous Exceptions
    CommandException (..),
    SomeExceptions (..),
    NotSupportedException (..),
    NoActionsRunException (..),

    -- * Optics
    _MkCommandException,
    _MkSomeExceptions,
    _MkNotSupportedException,
  )
where

import Data.Text qualified as T
import Pythia.Data.Command (Command (..))
import Pythia.Prelude

-- $setup
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Pythia.Prelude

-- | Exceptions encountered while running a shell command.
--
-- ==== __Examples__
-- >>> putStrLn $ displayException $ MkCommandException "some command" "an error message"
-- Command exception. Command: <some command>. Error: <an error message>
--
-- @since 0.1
type CommandException :: Type
data CommandException = MkCommandException Command Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''CommandException

-- | @since 0.1
instance Exception CommandException where
  displayException e =
    mconcat
      [ "Command exception. Command: <",
        T.unpack $ e' ^. _1 % #unCommand,
        ">. Error: <",
        T.unpack $ e' ^. _2,
        ">"
      ]
    where
      e' = e ^. _MkCommandException

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
newtype SomeExceptions = MkSomeExceptions (NonEmpty SomeException)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''SomeExceptions

-- | @since 0.1
instance Exception SomeExceptions where
  displayException (MkSomeExceptions xs) =
    mconcat
      [ "Encountered ",
        show $ length xs,
        " exception(s):",
        foldl' foldExs "" xs
      ]
    where
      foldExs acc ex = ("\n\n- " <> displayException ex) <> acc

-- | @since 0.1
instance Semigroup SomeExceptions where
  MkSomeExceptions l <> MkSomeExceptions r = MkSomeExceptions $ l <> r

-- | Error for when the current app is not supported.
--
-- ==== __Examples__
-- >>> putStrLn $ displayException $ MkNotSupportedException "app1"
-- App not supported: <app1>
--
-- @since 0.1
type NotSupportedException :: Type
newtype NotSupportedException = MkNotSupportedException Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''NotSupportedException

-- | @since 0.1
instance Exception NotSupportedException where
  displayException e =
    mconcat
      [ "App not supported: <",
        T.unpack (e ^. _MkNotSupportedException),
        ">"
      ]

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
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception NoActionsRunException where
  displayException _ = "No actions run"
