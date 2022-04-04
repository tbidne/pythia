{-# LANGUAGE DeriveAnyClass #-}

-- | Provides the 'PythiaException' type.
--
-- @since 0.1.0.0
module Pythia.Control.Exception
  ( -- * Main Exception Type
    PythiaException (..),

    -- * Functions
    -- $functions
    uncheckPythia,
    rethrowPythia,

    -- * Hierarchy
    -- $hierarchy
    pythiaExToException,
    pythiaExFromException,
  )
where

import Data.Typeable (cast)
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter)

-- | The root of our error hierarchy.
--
-- @since 0.1.0.0
data PythiaException = forall e. Exception e => MkPythiaException e
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )

-- | @since 0.1.0.0
deriving stock instance Show PythiaException

-- | @since 0.1.0.0
deriving anyclass instance PrettyPrinter PythiaException

-- $functions
-- These functions are provided as a convenience for working with
-- 'PythiaException'. They allow us to uncheck a checked 'PythiaException'
-- and rethrow an exception as a 'PythiaException'.

-- | 'uncheck' specialized to 'PythiaException'.
--
-- @since 0.1.0.0
uncheckPythia :: ((Throws PythiaException) => m a) -> m a
uncheckPythia = uncheck (Proxy @PythiaException)

-- | Rethrows a checked exception as a 'PythiaException'.
--
-- @since 0.1.0.0
rethrowPythia :: forall e m a. (Exception e, MonadCatch m, Throws PythiaException) => (Throws e => m a) -> m a
rethrowPythia = handle (\(ex :: e) -> throw $ MkPythiaException ex)

-- $hierarchy
-- These functions exist for defining other exceptions as part of the
-- 'PythiaException' hierarchy.

-- | Converts any 'Exception' to a 'SomeException' via 'PythiaException'.
--
-- @since 0.1.0.0
pythiaExToException :: Exception e => e -> SomeException
pythiaExToException = toException . MkPythiaException

-- | Converts any 'SomeException' to an 'Exception' via 'PythiaException'.
--
-- @since 0.1.0.0
pythiaExFromException :: Exception e => SomeException -> Maybe e
pythiaExFromException x = do
  MkPythiaException a <- fromException x
  cast a
