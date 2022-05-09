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
  )
where

import Data.Bytes (Bytes (..), Size (..))
import Data.Bytes qualified as Bytes
import Numeric.Algebra (MGroup, Normed)
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonNegative (NonNegative (..), unNonNegative)
import Numeric.Data.Positive (Positive (..), unPositive)
import Pythia.Data.RunApp (RunApp)
import Pythia.Data.Supremum (Supremum (..))
import Pythia.Prelude
import Pythia.Utils (Doc, Pretty (..), (<+>))
import Text.Printf qualified as Pf

-- | Determines how we should query the system for memory usage.
--
-- @since 0.1
type MemoryApp :: Type
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
type MemoryConfig :: Type
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

-- | Represents the current memory usage. The type parameter is some wrapper
-- around the memory intended to enforce an invariant e.g. non-negative.
--
-- @since 0.1
type Memory :: (Type -> Type) -> Type
newtype Memory (f :: Type -> Type) = MkMemory
  { -- | @since 0.1
    unMemory :: Bytes 'B (f Double)
  }
  deriving stock
    ( -- | @since 0.1
      Generic
    )

-- | @since 0.1
deriving stock instance Eq (f Double) => Eq (Memory f)

-- | @since 0.1
deriving stock instance Show (f Double) => Show (Memory f)

-- | @since 0.1
deriving anyclass instance NFData (f Double) => NFData (Memory f)

-- | @since 0.1
instance Pretty (Memory NonNegative) where
  pretty = prettyMemory unNonNegative

-- | @since 0.1
instance Pretty (Memory Positive) where
  pretty = prettyMemory unPositive

prettyMemory ::
  ( MGroup (f Double),
    Ord (f Double),
    Normed (f Double),
    NumLiteral (f Double)
  ) =>
  (f Double -> Double) ->
  Memory f ->
  Doc ann
prettyMemory unwrap (MkMemory bytes) = rounded <+> pretty (show sz)
  where
    bytes' = Bytes.normalize bytes
    sz = Bytes.someSizeToSize bytes'
    x = unwrap $ Bytes.unSomeSize bytes'
    rounded = pretty $ Pf.printf @(Double -> String) "%.2f" x

-- | @since 0.1
makeFieldLabelsNoPrefix ''Memory

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

-- | @since 0.1
makeFieldLabelsNoPrefix ''SystemMemory
