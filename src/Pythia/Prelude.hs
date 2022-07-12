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
    throwMaybe,
    showt,

    -- * Base
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..))
import Control.DeepSeq as X (NFData)
import Control.Exception.Safe as X
  ( Exception (..),
    SomeException,
    catch,
    catchAny,
    catchAnyDeep,
    handle,
    handleAny,
    throwIO,
    try,
  )
import Control.Monad as X (Monad (..), join, void, (<=<), (=<<), (>=>))
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
import Data.Tuple as X (uncurry)
import Data.Void as X (Void)
import Data.Word as X (Word8)
import GHC.Enum as X (Bounded (..), Enum (..))
import GHC.Err as X (error, undefined)
import GHC.Float as X (Double, Float)
import GHC.Generics as X (Generic)
import GHC.Num as X (Num (..))
import GHC.Read as X (Read (..))
import GHC.Real as X (even, floor, (/))
import GHC.Show as X (Show (..))
import Optics.Core as X
  ( over,
    view,
    (%),
    (%~),
    (.~),
    (^.),
    (^?),
    _1,
    _2,
    _Left,
    _Right,
  )
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrisms)
import System.IO as X (FilePath, IO, print, putStrLn)

-- $setup
-- >>> :set -XDeriveAnyClass
-- >>> data AnException = AnException deriving (Exception, Show)

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1
readFileUtf8Lenient :: FilePath -> IO Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . BS.readFile
{-# INLINEABLE readFileUtf8Lenient #-}

-- | Lenient UTF8 decode.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TextEnc.decodeUtf8With TextEncErr.lenientDecode
{-# INLINEABLE decodeUtf8Lenient #-}

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
{-# INLINEABLE headMaybe #-}

-- | Throws 'Left'.
--
-- ==== __Examples__
--
-- >>> throwLeft (Left @AnException @() AnException)
-- *** Exception: AnException
--
-- >>> throwLeft (Right @AnException @() ())
--
-- @since 0.1
throwLeft :: forall e a. Exception e => Either e a -> IO a
throwLeft = either throwIO pure
{-# INLINEABLE throwLeft #-}

-- | @throwMaybe e x@ throws @e@ if @x@ is 'Nothing'.
--
-- ==== __Examples__
--
-- >>> throwMaybe AnException Nothing
-- *** Exception: AnException
--
-- >>> throwMaybe AnException (Just ())
--
-- @since 0.1
throwMaybe :: forall e a. Exception e => e -> Maybe a -> IO a
throwMaybe e = maybe (throwIO e) pure
{-# INLINEABLE throwMaybe #-}

-- | 'Text' version of 'show'.
--
-- @since 0.1
showt :: Show a => a -> Text
showt = T.pack . show
{-# INLINEABLE showt #-}
