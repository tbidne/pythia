{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the core types describing the memory.
--
-- @since 0.1
module Pythia.Services.Memory.Types
  ( MemoryApp (..),
    MemoryConfig (..),
    _MkMemoryConfig,
    Memory (..),
    _MkMemory,
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
import Numeric.Algebra (MGroup, Normed)
import Numeric.Data.NonNegative (NonNegative (..), unNonNegative)
import Numeric.Data.Positive (Positive (..), unPositive)
import Numeric.Literal.Integer (FromInteger (..))
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Utils (Doc, Pretty (..), (<+>))

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

-- | Memory configuration.
--
-- >>> mempty @MemoryConfig
-- MkMemoryConfig Many
--
-- @since 0.1
type MemoryConfig :: Type
newtype MemoryConfig = MkMemoryConfig (RunApp MemoryApp)
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
makePrisms ''MemoryConfig

-- | @since 0.1
instance Semigroup MemoryConfig where
  MkMemoryConfig l <> MkMemoryConfig r = MkMemoryConfig (l <> r)
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Monoid MemoryConfig where
  mempty = MkMemoryConfig mempty
  {-# INLINEABLE mempty #-}

-- | Represents the current memory usage. The type parameter is some wrapper
-- around the memory intended to enforce an invariant e.g. non-negative.
--
-- @since 0.1
type Memory :: (Type -> Type) -> Type
newtype Memory (f :: Type -> Type) = MkMemory (Bytes 'B (f Double))
  deriving stock
    ( -- | @since 0.1
      Generic
    )

-- | @since 0.1
makePrisms ''Memory

-- | @since 0.1
deriving stock instance Eq (f Double) => Eq (Memory f)

-- | @since 0.1
deriving stock instance Show (f Double) => Show (Memory f)

-- | @since 0.1
deriving anyclass instance NFData (f Double) => NFData (Memory f)

-- | @since 0.1
instance Pretty (Memory NonNegative) where
  pretty = prettyMemory unNonNegative
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Pretty (Memory Positive) where
  pretty = prettyMemory unPositive
  {-# INLINEABLE pretty #-}

prettyMemory ::
  ( FromInteger (f Double),
    MGroup (f Double),
    Ord (f Double),
    Normed (f Double)
  ) =>
  (f Double -> Double) ->
  Memory f ->
  Doc ann
prettyMemory unwrapper (MkMemory bytes) = pretty formatted
  where
    bytes' = Bytes.normalize bytes
    unwrapped = fmap unwrapper bytes'
    formatted = formatSized (MkFloatingFormatter (Just 2)) sizedFormatterUnix unwrapped
{-# INLINEABLE prettyMemory #-}

-- | Represents the current memory usage.
--
-- @since 0.1
type SystemMemory :: Type
data SystemMemory = MkSystemMemory
  { -- | The total memory on this system.
    --
    -- @since 0.1
    total :: !(Memory Positive),
    -- | The memory currently in use. This does not include the
    -- cache.
    --
    -- @since 0.1
    used :: !(Memory NonNegative)
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
makeFieldLabelsNoPrefix ''SystemMemory
