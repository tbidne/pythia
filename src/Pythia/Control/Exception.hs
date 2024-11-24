-- | Provides various exception types.
--
-- @since 0.1
module Pythia.Control.Exception
  ( -- * Root
    PythiaException (..),
    toPythiaException,
    fromPythiaException,

    -- * Miscellaneous
    CommandException (..),
    SomeExceptions (..),
    NotSupportedException (..),
    NoActionsRunException (..),
  )
where

import Data.Text qualified as T
import Data.Typeable (cast)
import GHC.Show (Show (showsPrec), showParen)
import GHC.Show qualified as Sh
import Pythia.Data.Command (Command)
import Pythia.Prelude

-- $setup
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Pythia.Prelude

-- | Exception hierarchy root for exceptions thrown by this library.
--
-- @since 0.1
type PythiaException :: Type
data PythiaException = forall e. (Exception e) => MkPythiaException e

-- | @since 0.1
instance Show PythiaException where
  showsPrec i (MkPythiaException ex) =
    showParen
      (i >= Sh.appPrec1)
      ( Sh.showString "MkPythiaException "
          . Sh.showsPrec Sh.appPrec1 ex
      )

-- | @since 0.1
instance Exception PythiaException where
  displayException (MkPythiaException ex) =
    "Pythia exception: " <> displayException ex

-- | 'toException' via 'PythiaException'. Used for defining an exception
-- as a subtype of 'PythiaException'.
--
-- @since 0.1
toPythiaException :: (Exception e) => e -> SomeException
toPythiaException = toException . MkPythiaException

-- | 'fromException' via 'PythiaException'. Used for defining an exception
-- as a subtype of 'PythiaException'.
--
-- @since 0.1
fromPythiaException :: (Exception e) => SomeException -> Maybe e
fromPythiaException x = do
  MkPythiaException e <- fromException x
  cast e

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
instance Exception CommandException where
  displayException (MkCommandException c t) =
    mconcat
      [ "Command exception. Command: <",
        T.unpack $ c ^. #unCommand,
        ">. Error: <",
        T.unpack t,
        ">"
      ]

  toException = toPythiaException
  fromException = fromPythiaException

-- | Collects 1 or more exceptions.
--
-- ==== __Examples__
-- >>> let ex1 = toException $ MkCommandException "some command" "an error message"
-- >>> let ex2 = toException $ MkNotSupportedException "app1"
-- >>> displayException $ MkSomeExceptions $ ex1 :| [ex2]
-- "Encountered 2 exception(s):\n\n- App not supported: <app1>\n\n- Command exception. Command: <some command>. Error: <an error message>"
--
-- @since 0.1
type SomeExceptions :: Type
newtype SomeExceptions = MkSomeExceptions (NonEmpty SomeException)
  deriving stock
    ( -- | @since 0.1
      Show
    )

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

  toException = toPythiaException
  fromException = fromPythiaException

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
instance Exception NotSupportedException where
  displayException (MkNotSupportedException e) =
    mconcat
      [ "App not supported: <",
        T.unpack e,
        ">"
      ]

  toException = toPythiaException
  fromException = fromPythiaException

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

  toException = toPythiaException
  fromException = fromPythiaException
