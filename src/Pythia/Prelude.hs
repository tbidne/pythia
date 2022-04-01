-- | Custom prelude.
--
-- @since 0.1.0.0
module Pythia.Prelude
  ( readFileUtf8Lenient,
    readFileUtf8LenientEither,
    decodeUtf8Lenient,
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..))
import Control.Exception.Safe as X (Exception (..), SomeException (..), throw, try)
import Control.Monad as X (Monad (..), join, void, (<=<), (>=>))
import Control.Monad.IO.Class as X (MonadIO (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.ByteString qualified as BS
import Data.Char as X (Char)
import Data.Either as X (Either (..), either)
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..), for_)
import Data.Function as X (const, id, ($), (.))
import Data.Functor as X (Functor (..), ($>), (<$>))
import Data.Int as X (Int)
import Data.Kind as X (Type)
import Data.List as X (filter)
import Data.Maybe as X (Maybe (..), fromMaybe, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..))
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (IsString (..), String)
import Data.Text as X (Text)
import Data.Text.Encoding qualified as TextEnc
import Data.Text.Encoding.Error qualified as TextEncErr
import Data.Traversable as X (Traversable (..), for)
import Data.Void as X (Void)
import GHC.Enum as X (Bounded (..), Enum (..))
import GHC.Err as X (undefined)
import GHC.Read as X (Read (..))
import GHC.Show as X (Show (..))
import Optics.Core as X (view, (%), (%~), (.~), (^.), (^?), _Left, _Right)
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrismLabels)
import System.IO as X (FilePath, IO, print, putStrLn)

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1.0.0
readFileUtf8Lenient :: FilePath -> IO Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . BS.readFile

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1.0.0
readFileUtf8LenientEither :: FilePath -> IO (Either SomeException Text)
readFileUtf8LenientEither fp = do
  eByteString :: Either SomeException ByteString <- try $ BS.readFile fp
  case eByteString of
    Left ex -> pure $ Left ex
    Right bs -> pure $ Right $ decodeUtf8Lenient bs

-- | Lenient UTF8 decode.
--
-- @since 0.1.0.0
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TextEnc.decodeUtf8With TextEncErr.lenientDecode
