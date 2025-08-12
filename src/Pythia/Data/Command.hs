{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the 'Command' type.
--
-- @since 0.1
module Pythia.Data.Command
  ( Command (..),
  )
where

import Pythia.Prelude

-- | Newtype wrapper over a shell command.
--
-- @since 0.1
type Command :: Type
newtype Command = MkCommand
  { unCommand :: Text
  }
  deriving stock
    ( -- | @since 0.1
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
      IsString
    )
    via Text
  deriving anyclass
    ( -- | @since 0.1.0.0
      Hashable,
      -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unCommand" k Command Command a b
  where
  labelOptic = iso (\(MkCommand t) -> t) MkCommand
  {-# INLINE labelOptic #-}
