{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the memory.
--
-- @since 0.1
module Pythia.Services.Memory.Types
  ( MemoryApp (..),
    MemoryConfig (..),
    Memory (..),
  )
where

import Data.Bytes (Bytes (..), Size (..))
import Data.Bytes qualified as Bytes
import Numeric.Data.NonNegative (NonNegative (..))
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Utils (Doc, Pretty (..), (<+>))
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
data Memory = MkMemory
  { -- | The total memory on this system.
    --
    -- @since 0.1
    total :: Bytes 'B (NonNegative Double),
    -- | The memory currently in use. This does not include the
    -- cache.
    --
    -- @since 0.1
    used :: Bytes 'B (NonNegative Double)
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
  pretty :: forall ann. Memory -> Doc ann
  pretty mem = u <+> "/" <+> t
    where
      t = f $ mem ^. #total
      u = f $ mem ^. #used
      f :: Bytes 'B (NonNegative Double) -> Doc ann
      f bytes = rounded <+> pretty (show sz)
        where
          bytes' = Bytes.normalize bytes
          sz = Bytes.someSizeToSize bytes'
          (MkNonNegative x) = Bytes.unSomeSize bytes'
          rounded = pretty $ Pf.printf @(Double -> String) "%.2f" x

-- | @since 0.1
makeFieldLabelsNoPrefix ''Memory
