{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides common network types.
--
-- @since 0.1.0.0
module System.Info.Services.Network.Types
  ( Device (..),
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Optics.TH qualified as OTH

-- | Newtype wrapper over a network device name.
--
-- @since 0.1.0.0
newtype Device = MkDevice
  { -- | @since 0.1.0.0
    unDevice :: Text
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

OTH.makeFieldLabelsNoPrefix ''Device