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
    MemoryApp (..),
  )
where

import Data.Bytes (Bytes (..), _MkBytes)
import Numeric.Data.Interval (LRInterval (..))
import Numeric.Data.Interval qualified as Interval
import Numeric.Data.NonNegative (NonNegative (..), _MkNonNegative)
import Numeric.Data.NonNegative qualified as NN
import Numeric.Data.Positive (_MkPositive)
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Prelude
import Pythia.Services.Memory.Free qualified as Free
import Pythia.Services.Memory.Types
  ( Memory (..),
    MemoryApp (..),
    SystemMemory (..),
    _MkMemory,
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
queryMemory :: MemoryApp -> IO SystemMemory
queryMemory MemoryAppFree = Free.memoryShellApp

-- | Returns the amount of free memory.
--
-- @since 0.1
freeMemory :: SystemMemory -> Memory NonNegative
freeMemory sysMem = free
  where
    t = sysMem ^. (#total % _MkMemory % _MkBytes % _MkPositive)
    u = sysMem ^. (#used % _MkMemory % _MkBytes % _MkNonNegative)
    free = MkMemory $ MkBytes $ NN.unsafeNonNegative $ t - u

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

-- | Returns the free memory as a percentage.
--
-- @since 0.1
percentageFree :: SystemMemory -> Percentage
percentageFree sysMem = MkPercentage $ Interval.unsafeLRInterval (100 - usedPercent)
  where
    (MkPercentage (MkLRInterval usedPercent)) = percentageUsed sysMem
