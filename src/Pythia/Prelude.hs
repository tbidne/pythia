-- | Custom prelude.
--
-- @since 0.1
module Pythia.Prelude
  ( -- * File
    readFileUtf8Lenient,
    decodeUtf8Lenient,

    -- * Misc
    headMaybe,
    throwLeft,
    showt,

    -- * Base
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..))
import Control.Exception as X (Exception (..), SomeException (..))
import Control.Exception.Safe as X
  ( MonadCatch,
    MonadThrow,
    catch,
    handle,
    throw,
    try,
  )
import Control.Monad as X (Monad (..), join, void, (<=<), (=<<), (>=>))
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
import Data.Proxy as X (Proxy (..))
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (IsString (..), String)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEnc
import Data.Text.Encoding.Error qualified as TextEncErr
import Data.Traversable as X (Traversable (..), for)
import Data.Void as X (Void)
import GHC.Enum as X (Bounded (..), Enum (..))
import GHC.Err as X (error, undefined)
import GHC.Read as X (Read (..))
import GHC.Real as X (even)
import GHC.Show as X (Show (..))
import Optics.Core as X (over, view, (%), (%~), (.~), (^.), (^?), _Left, _Right)
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrismLabels)
import System.IO as X (FilePath, IO, print, putStrLn)

-- $setup
-- >>> :set -XDeriveAnyClass
-- >>> data AnException = AnException deriving (Exception, Show)

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1
readFileUtf8Lenient :: MonadIO m => FilePath -> m Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . liftIO . BS.readFile

-- | Lenient UTF8 decode.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TextEnc.decodeUtf8With TextEncErr.lenientDecode

-- | Total version of 'Prelude.head'.
--
-- ==== __Examples__
--
-- >>> headMaybe []
-- Nothing
--
-- >>> headMaybe [3, 4]
-- Just 3
--
-- @since 0.1
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Throws 'Left'.
--
-- ==== __Examples__
--
-- >>> throwLeft @Maybe (Left @AnException @() AnException)
-- Nothing
--
-- >>> throwLeft @Maybe (Right @AnException @() ())
-- Just ()
--
-- @since 0.1
throwLeft :: forall m e a. (Exception e, MonadThrow m) => Either e a -> m a
throwLeft = either throw pure

showt :: Show a => a -> Text
showt = T.pack . show
