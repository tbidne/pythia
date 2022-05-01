{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the memory.
--
-- @since 0.1
module Pythia.Services.Memory.Types
  ( MemoryApp (..),
    MemoryConfig (..),
    Memory (..),
    SystemMemory (..),
    diffMemory,
  )
where

import Data.Bytes (Bytes (..), Size (..))
import Data.Bytes qualified as Bytes
import Numeric.Data.NonNegative (NonNegative (..))
import Numeric.Data.NonNegative qualified as NN
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Utils (Pretty (..), (<+>))
import Text.Printf qualified as Pf

-- | Determines how we should query the system for memory usage.
--
-- @since 0.1
data MemoryApp
  = -- | Uses the free utility.
    --
    -- @since 0.1
    MemoryFree
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup
    )
    via (Supremum MemoryApp)
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
makePrismLabels ''MemoryApp

-- | Memory configuration.
--
-- >>> mempty @MemoryConfig
-- MkMemoryConfig {memoryApp = Many}
--
-- @since 0.1
newtype MemoryConfig = MkMemoryConfig
  { -- | @since 0.1
    memoryApp :: RunApp MemoryApp
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''MemoryConfig

-- | @since 0.1
instance Semigroup MemoryConfig where
  MkMemoryConfig l <> MkMemoryConfig r = MkMemoryConfig (l <> r)

-- | @since 0.1
instance Monoid MemoryConfig where
  mempty = MkMemoryConfig mempty

-- | Represents the current memory usage.
--
-- @since 0.1
newtype Memory = MkMemory
  { -- | @since 0.1
    unMemory :: Bytes 'B (NonNegative Double)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Pretty Memory where
  pretty (MkMemory bytes) = rounded <+> pretty (show sz)
    where
      bytes' = Bytes.normalize bytes
      sz = Bytes.someSizeToSize bytes'
      (MkNonNegative x) = Bytes.unSomeSize bytes'
      rounded = pretty $ Pf.printf @(Double -> String) "%.2f" x

-- | @since 0.1
makeFieldLabelsNoPrefix ''Memory

-- | Returns the absolute difference.
--
-- @since 0.1
diffMemory :: Memory -> Memory -> Memory
diffMemory m1 m2
  | x1 > x2 = MkMemory $ MkBytes @'B (NN.unsafeNonNegative (x1 - x2))
  | otherwise = MkMemory $ MkBytes @'B (NN.unsafeNonNegative (x2 - x1))
  where
    x1 = unMem m1
    x2 = unMem m2
    unMem = NN.unNonNegative . unBytes . unMemory

-- | Represents the current memory usage.
--
-- @since 0.1
data SystemMemory = MkSystemMemory
  { -- | The total memory on this system.
    --
    -- @since 0.1
    total :: Memory,
    -- | The memory currently in use. This does not include the
    -- cache.
    --
    -- @since 0.1
    used :: Memory
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Pretty SystemMemory where
  pretty mem = pretty t <+> "/" <+> pretty u
    where
      t = mem ^. #total
      u = mem ^. #used

-- | @since 0.1
makeFieldLabelsNoPrefix ''SystemMemory
