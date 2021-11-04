-- | This module provides common data types.
module System.Info.Data
  ( Command (..),
    QueryError (..),
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

-- | Core error type.
data QueryError = MkQueryError
  { -- | The name associated to the error (usually a module name).
    name :: Text,
    -- | A short description of the error.
    short :: Text,
    -- | More detailed description.
    long :: Text
  }
  deriving (Eq, Show)

instance LabelOptic "name" A_Lens QueryError QueryError Text Text where
  labelOptic = O.lens name (\err name' -> err {name = name'})

instance LabelOptic "short" A_Lens QueryError QueryError Text Text where
  labelOptic = O.lens short (\err short' -> err {short = short'})

instance LabelOptic "long" A_Lens QueryError QueryError Text Text where
  labelOptic = O.lens long (\err long' -> err {long = long'})
