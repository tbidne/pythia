{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides common data types.
--
-- @since 0.1.0.0
module Pythia.Data
  ( Command (..),
    RunApp (..),
  )
where

import Pythia.Prelude

-- | Newtype wrapper over a shell command.
--
-- @since 0.1.0.0
newtype Command = MkCommand
  { -- | @since 0.1.0.0
    unCommand :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      IsString
    )
    via Text

makeFieldLabelsNoPrefix ''Command

-- | @since 0.1.0.0
data RunApp a
  = -- | @since 0.1.0.0
    Many
  | -- | @since 0.1.0.0
    Single a
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

makePrismLabels ''RunApp

instance Semigroup a => Semigroup (RunApp a) where
  Many <> r = r
  l <> Many = l
  Single l <> Single r = Single (l <> r)

instance Semigroup a => Monoid (RunApp a) where
  mempty = Many
