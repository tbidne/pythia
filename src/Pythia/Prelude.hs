-- | Custom prelude.
--
-- @since 0.1.0.0
module Pythia.Prelude
  ( -- * File
    readFileUtf8Lenient,
    decodeUtf8Lenient,

    -- * Unchecking exceptions
    uncheck2,
    uncheck3,
    uncheck4,
    uncheck5,

    -- * Base
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..))
import Control.Exception as X (Exception (..), SomeException (..))
import Control.Exception.Safe.Checked as X (MonadCatch, Throws, catch, throw, try, uncheck)
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
import Data.List as X (filter, replicate)
import Data.Maybe as X (Maybe (..), fromMaybe, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..))
import Data.Proxy (Proxy (..))
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
import GHC.Real as X (even)
import GHC.Show as X (Show (..))
import Optics.Core as X (view, (%), (%~), (.~), (^.), (^?), _Left, _Right)
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrismLabels)
import System.IO as X (FilePath, IO, print, putStrLn)

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1.0.0
readFileUtf8Lenient :: MonadIO m => FilePath -> m Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . liftIO . BS.readFile

-- | Lenient UTF8 decode.
--
-- @since 0.1.0.0
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TextEnc.decodeUtf8With TextEncErr.lenientDecode

-- | Uncheck two checked exceptions.
--
-- @since 0.1.0.0
uncheck2 :: forall e f a. ((Throws e, Throws f) => a) -> a
uncheck2 x = uncheck (Proxy @e) $ uncheck (Proxy @f) x

-- | Uncheck 2 checked exceptions.
--
-- @since 0.1.0.0
uncheck3 :: forall e f g a. ((Throws e, Throws f, Throws g) => a) -> a
uncheck3 x = uncheck (Proxy @e) $ uncheck2 @f @g x

-- | Uncheck 4 checked exceptions.
--
-- @since 0.1.0.0
uncheck4 :: forall e f g h a. ((Throws e, Throws f, Throws g, Throws h) => a) -> a
uncheck4 x = uncheck (Proxy @e) $ uncheck3 @f @g @h x

-- | Uncheck 5 checked exceptions.
--
-- @since 0.1.0.0
uncheck5 :: forall e f g h i a. ((Throws e, Throws f, Throws g, Throws h, Throws i) => a) -> a
uncheck5 x = uncheck (Proxy @e) $ uncheck4 @f @g @h @i x
