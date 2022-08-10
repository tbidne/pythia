{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the 'Command' type.
--
-- @since 0.1
module Pythia.Data.Command
  ( Command (..),
    _MkCommand,
  )
where

import Pythia.Prelude

-- | Newtype wrapper over a shell command.
--
-- @since 0.1
type Command :: Type
newtype Command = MkCommand Text
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
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''Command
