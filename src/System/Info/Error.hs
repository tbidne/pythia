-- | This module provides the 'Error' type.
module System.Info.Error
  ( Error (..),
  )
where

import Data.Text (Text)
import Optics.Core (A_Lens, LabelOptic (..))
import Optics.Core qualified as O

-- | Core error type.
data Error = MkError
  { -- | The name associated to the error (usually a module name).
    name :: Text,
    -- | A short description of the error.
    short :: Text,
    -- | More detailed description.
    long :: Text
  }
  deriving (Eq, Show)

instance LabelOptic "name" A_Lens Error Error Text Text where
  labelOptic = O.lens name (\err name' -> err {name = name'})

instance LabelOptic "short" A_Lens Error Error Text Text where
  labelOptic = O.lens short (\err short' -> err {short = short'})

instance LabelOptic "long" A_Lens Error Error Text Text where
  labelOptic = O.lens long (\err long' -> err {long = long'})
