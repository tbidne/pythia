-- | Custom prelude.
--
-- @since 0.1.0.0
module Pythia.Prelude
  ( readFileUtf8Lenient,
    readFileUtf8LenientExceptT,
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..))
import Control.Exception.Safe as X (SomeException, try)
import Control.Monad as X (Monad (..), join, void, (<=<), (>=>))
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.Trans.Except as X (ExceptT (..), throwE)
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.ByteString qualified as BS
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
import Data.Text.Encoding qualified as TextEnc
import Data.Text.Encoding.Error qualified as TextEncErr
import Data.Traversable as X (Traversable (..), for)
import Data.Void as X (Void)
import GHC.Err as X (undefined)
import GHC.Show as X (Show (..))
import Optics.Core as X ((%), (%~), (.~), (^.), (^?))
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrismLabels)
import System.IO as X (FilePath, IO, putStrLn)

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1.0.0
readFileUtf8Lenient :: FilePath -> IO Text
readFileUtf8Lenient =
  fmap (TextEnc.decodeUtf8With TextEncErr.lenientDecode)
    . BS.readFile

-- | 'readFileUtf8Lenient' in 'ExceptT'.
--
-- @since 0.1.0.0
readFileUtf8LenientExceptT :: (SomeException -> e) -> FilePath -> ExceptT e IO Text
readFileUtf8LenientExceptT errFn fp = do
  eByteString <- liftIO $ try (BS.readFile fp)
  case eByteString of
    Left ex -> throwE $ errFn ex
    Right bs -> pure $ TextEnc.decodeUtf8With TextEncErr.lenientDecode bs
