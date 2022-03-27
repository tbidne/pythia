-- | Custom prelude.
--
-- @since 0.1.0.0
module Pythia.Prelude
  ( module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..))
import Control.Monad as X (Monad (..))
import Data.Bool as X (Bool (..), not, otherwise)
import Data.Either as X (Either (..))
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..))
import Data.Function as X (id, ($), (.))
import Data.Functor as X (Functor (..), ($>), (<$>))
import Data.Int as X (Int)
import Data.List as X (filter)
import Data.Maybe as X (Maybe (..), maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..))
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (String)
import Data.Text as X (Text)
import GHC.Show as X (Show (..))
import Optics.Core as X ((%), (%~), (.~), (^.), (^?))
import System.IO as X (IO, putStrLn)
