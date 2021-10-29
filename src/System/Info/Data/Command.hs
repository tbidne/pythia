-- | This module provides the 'Command' type.
module System.Info.Data.Command
  ( Command (..),
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Optics.Core (A_Lens, LabelOptic)
import Optics.Core qualified as O

-- | Newtype wrapper over a shell command
newtype Command = MkCommand {unCommand :: Text}
  deriving (Eq, Ord, Show)
  deriving (IsString) via Text

instance LabelOptic "unCommand" A_Lens Command Command Text Text where
  labelOptic = O.lens unCommand (\cmd t -> cmd {unCommand = t})
