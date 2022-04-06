{-# LANGUAGE TemplateHaskell #-}
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
newtype Command = MkCommand
  { -- | @since 0.1
    unCommand :: Text
  }
  deriving
    ( -- | @since 0.1
      Eq,
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

-- | @since 0.1
makeFieldLabelsNoPrefix ''Command
