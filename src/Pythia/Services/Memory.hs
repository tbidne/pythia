-- | This module exports memory related services.
--
-- @since 0.1
module Pythia.Services.Memory
  ( -- * Queries
    queryMemory,

    -- * Functions
    freeMemory,
    percentageUsed,
    percentageFree,

    -- * Types
    Memory (..),
    SystemMemory (..),

    -- ** Configuration
    MemoryConfig (..),
    MemoryApp (..),
    RunApp (..),

    -- ** Errors
    FreeParseError (..),

    -- * Optics

    -- ** Types
    _MkMemory,

    -- ** Configuration
    _MkMemoryConfig,

    -- ** Errors
    _MkFreeParseError,
  )
where

import Data.Bytes (Bytes (..), _MkBytes)
import Numeric.Data.Interval (LRInterval (..))
import Numeric.Data.Interval qualified as Interval
import Numeric.Data.NonNegative (NonNegative (..), _MkNonNegative)
import Numeric.Data.NonNegative qualified as NN
import Numeric.Data.Positive (_MkPositive)
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Internal.ShellApp (AppAction (..))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.Memory.Free (FreeParseError (..), _MkFreeParseError)
import Pythia.Services.Memory.Free qualified as Free
import Pythia.Services.Memory.Types
  ( Memory (..),
    MemoryApp (..),
    MemoryConfig (..),
    SystemMemory (..),
    _MkMemory,
    _MkMemoryConfig,
  )

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

-- | Queries the memory based on the configuration. If 'app' is
-- 'Many' then we try supported apps in the following order:
--
-- @
-- ['MemoryFree']
-- @
--
-- __Throws:__
--
-- * 'FreeParseError'
-- * 'Pythia.Control.Exception.CommandException'
--
-- @since 0.1
queryMemory :: MemoryConfig -> IO SystemMemory
queryMemory config =
  case config ^. _MkMemoryConfig of
    Many -> ShellApp.tryAppActions allApps
    Single app -> toShellApp app
  where
    allApps =
      [ MkAppAction (toShellApp MemoryAppFree) Free.supported "free"
      ]
{-# INLINEABLE queryMemory #-}

toShellApp :: MemoryApp -> IO SystemMemory
toShellApp MemoryAppFree = Free.memoryShellApp
{-# INLINEABLE toShellApp #-}

-- | Returns the amount of free memory.
--
-- @since 0.1
freeMemory :: SystemMemory -> Memory NonNegative
freeMemory sysMem = free
  where
    t = sysMem ^. (#total % _MkMemory % _MkBytes % _MkPositive)
    u = sysMem ^. (#used % _MkMemory % _MkBytes % _MkNonNegative)
    free = MkMemory $ MkBytes $ NN.unsafeNonNegative $ t - u
{-# INLINEABLE freeMemory #-}

-- | Returns the used memory as a percentage.
--
-- @since 0.1
percentageUsed :: SystemMemory -> Percentage
percentageUsed sysMem = MkPercentage p
  where
    t = sysMem ^. (#total % _MkMemory % _MkBytes % _MkPositive)
    u = sysMem ^. (#used % _MkMemory % _MkBytes % _MkNonNegative)
    p = Interval.unsafeLRInterval $ doubleToWord8 $ u / t

    doubleToWord8 :: Double -> Word8
    doubleToWord8 = floor . (* 100)
{-# INLINEABLE percentageUsed #-}

-- | Returns the free memory as a percentage.
--
-- @since 0.1
percentageFree :: SystemMemory -> Percentage
percentageFree sysMem = MkPercentage $ Interval.unsafeLRInterval (100 - usedPercent)
  where
    (MkPercentage (MkLRInterval usedPercent)) = percentageUsed sysMem
{-# INLINEABLE percentageFree #-}
