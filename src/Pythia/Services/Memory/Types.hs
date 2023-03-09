{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the memory.
--
-- @since 0.1
module Pythia.Services.Memory.Types
  ( MemoryApp (..),
    Memory (..),
    SystemMemory (..),
  )
where

import Data.Bytes (Bytes (..), Size (..))
import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting
  ( FloatingFormatter (..),
    formatSized,
    sizedFormatterUnix,
  )
import Pythia.Prelude
import Pythia.Utils (Pretty (..), (<+>))

-- $setup
-- >>> import Pythia.Prelude

-- | Determines how we should query the system for memory usage.
--
-- @since 0.1
type MemoryApp :: Type
data MemoryApp
  = -- | Uses the free utility.
    --
    -- @since 0.1
    MemoryAppFree
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
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | Represents the current memory usage. The type parameter is some wrapper
-- around the memory intended to enforce an invariant e.g. non-negative.
--
-- @since 0.1
type Memory :: Type
newtype Memory = MkMemory {unMemory :: Bytes 'B Natural}
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
instance
  (k ~ An_Iso, a ~ Bytes 'B Natural, b ~ Bytes 'B Natural) =>
  LabelOptic "unMemory" k Memory Memory a b
  where
  labelOptic = iso (\(MkMemory p) -> p) MkMemory
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Pretty Memory where
  pretty (MkMemory bytes) = pretty formatted
    where
      bytes' = Bytes.normalize $ fmap natToDouble bytes
      formatted = formatSized (MkFloatingFormatter (Just 2)) sizedFormatterUnix bytes'
  {-# INLINEABLE pretty #-}

-- | Represents the current memory usage.
--
-- @since 0.1
type SystemMemory :: Type
data SystemMemory = MkSystemMemory
  { -- | The total memory on this system.
    --
    -- @since 0.1
    total :: !Memory,
    -- | The memory currently in use. This does not include the
    -- cache.
    --
    -- @since 0.1
    used :: !Memory
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
  pretty mem = pretty u <+> "/" <+> pretty t
    where
      t = mem ^. #total
      u = mem ^. #used
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Memory, b ~ Memory) =>
  LabelOptic "total" k SystemMemory SystemMemory a b
  where
  labelOptic = lensVL $ \f (MkSystemMemory _total _used) ->
    fmap (`MkSystemMemory` _used) (f _total)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Memory, b ~ Memory) =>
  LabelOptic "used" k SystemMemory SystemMemory a b
  where
  labelOptic = lensVL $ \f (MkSystemMemory _total _used) ->
    fmap (MkSystemMemory _total) (f _used)
  {-# INLINE labelOptic #-}
