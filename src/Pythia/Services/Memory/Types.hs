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

import ByteTypes.Bytes (Bytes (..), Size (..))
import ByteTypes.Bytes qualified as Bytes
import Data.Text qualified as T
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude

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
    total :: Bytes 'B Double,
    -- | The memory currently in use. This does not include the
    -- cache.
    --
    -- @since 0.1
    used :: Bytes 'B Double
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
instance PrettyPrinter Memory where
  pretty mem = u <> " / " <> t
    where
      t = f $ mem ^. #total
      u = f $ mem ^. #used
      f = T.pack . Bytes.pretty . Bytes.normalize

-- | @since 0.1
makeFieldLabelsNoPrefix ''Memory
