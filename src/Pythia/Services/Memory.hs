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
    FreeException (..),
  )
where

import Data.Bytes (Bytes (..))
import Numeric.Data.Interval (LRInterval (..))
import Numeric.Data.Interval qualified as Interval
import Numeric.Data.NonNegative (NonNegative (..))
import Numeric.Data.NonNegative qualified as NN
import Numeric.Data.Positive qualified as Pos
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Prelude
import Pythia.Services.Memory.Free (FreeException)
import Pythia.Services.Memory.Free qualified as Free
import Pythia.Services.Memory.Types
  ( Memory (..),
    MemoryApp (..),
    MemoryConfig (..),
    SystemMemory (..),
  )
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp

-- | Queries the memory based on the configuration. If 'memoryApp' is
-- 'Many' then we try supported apps in the following order:
--
-- @
-- ['MemoryFree']
-- @
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryMemory :: MonadUnliftIO m => MemoryConfig -> m SystemMemory
queryMemory config =
  case config ^. #memoryApp of
    Many -> ShellApp.tryAppActions allApps
    Single app -> toShellApp app
  where
    allApps =
      [ MkAppAction (toShellApp MemoryFree) Free.supported (showt MemoryFree)
      ]

toShellApp :: MonadUnliftIO m => MemoryApp -> m SystemMemory
toShellApp MemoryFree = Free.memoryShellApp

-- | Returns the amount of free memory.
--
-- @since 0.1
freeMemory :: SystemMemory -> Memory NonNegative
freeMemory sysMem = free
  where
    t = Pos.unPositive . unBytes . unMemory $ sysMem ^. #total
    u = NN.unNonNegative . unBytes . unMemory $ sysMem ^. #used
    free = MkMemory $ MkBytes $ NN.unsafeNonNegative $ t - u

-- | Returns the used memory as a percentage.
--
-- @since 0.1
percentageUsed :: SystemMemory -> Percentage
percentageUsed sysMem = MkPercentage p
  where
    t = Pos.unPositive . unBytes $ sysMem ^. #total % #unMemory
    u = NN.unNonNegative . unBytes $ sysMem ^. #used % #unMemory
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
