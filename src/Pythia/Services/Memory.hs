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

import Data.Bytes (Bytes (MkBytes), _MkBytes)
import Numeric.Data.Interval (LRInterval (MkLRInterval))
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.Percentage (Percentage (MkPercentage))
import Pythia.Prelude
import Pythia.Services.Memory.Free qualified as Free
import Pythia.Services.Memory.Types
  ( Memory (MkMemory),
    MemoryApp (MemoryAppFree),
    SystemMemory (MkSystemMemory, total, used),
  )

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

-- | Queries the memory based on the configuration.
--
-- @since 0.1
queryMemory :: MemoryApp -> IO SystemMemory
queryMemory MemoryAppFree = Free.memoryShellApp

-- | Returns the amount of free memory.
--
-- @since 0.1
freeMemory :: SystemMemory -> Memory
freeMemory sysMem = free
  where
    t = sysMem ^. (#total % #unMemory % _MkBytes)
    u = sysMem ^. (#used % #unMemory % _MkBytes)
    free = MkMemory $ MkBytes $ t - u

-- | Returns the used memory as a percentage.
--
-- @since 0.1
percentageUsed :: SystemMemory -> Percentage
percentageUsed sysMem = MkPercentage p
  where
    t = natToDouble $ sysMem ^. (#total % #unMemory % _MkBytes)
    u = natToDouble $ sysMem ^. (#used % #unMemory % _MkBytes)
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
