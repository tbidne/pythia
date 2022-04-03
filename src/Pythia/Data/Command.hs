{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the 'Command' type.
--
-- @since 0.1.0.0
module Pythia.Data.Command
  ( Command (..),
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

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''Command
