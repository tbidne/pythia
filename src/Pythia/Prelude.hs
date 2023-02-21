{-# LANGUAGE CPP #-}

-- | Custom prelude.
--
-- @since 0.1
module Pythia.Prelude
  ( -- * Misc
    headMaybe,
    throwLeft,
    throwMaybe,
    showt,
    natToDouble,

    -- * Base
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..))
import Control.DeepSeq as X (NFData)
import Control.Monad as X
  ( Monad (..),
    join,
    unless,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Char as X (Char)
import Data.Either as X (Either (..), either)
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..), for_)
import Data.Function as X (const, id, ($), (.))
import Data.Functor as X (Functor (..), ($>), (<$>), (<&>))
import Data.Int as X (Int)
import Data.Kind as X (Type)
import Data.List as X (filter, replicate)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (..), fromMaybe, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..))
import Data.Proxy as X (Proxy (..))
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (IsString (..), String)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Traversable as X (Traversable (..), for)
import Data.Tuple as X (uncurry)
import Effects.Exception as X
  ( Exception (..),
    SomeException,
    addCS,
    throwCS,
    throwM,
    tryAny,
  )
import Effects.FileSystem.FileReader as X
  ( decodeUtf8Lenient,
    readFileUtf8Lenient,
  )
import GHC.Natural as X (Natural)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality as X (type (~))
#endif
import Data.Void as X (Void)
import Data.Word as X (Word8)
import GHC.Enum as X (Bounded (..), Enum (..))
import GHC.Err as X (error, undefined)
import GHC.Float as X (Double, Float)
import GHC.Generics as X (Generic)
import GHC.Num as X (Num (..))
import GHC.Read as X (Read (..))
import GHC.Real as X (even, floor, fromIntegral, (/))
import GHC.Show as X (Show (..))
import Optics.Core as X
  ( A_Prism,
    An_Iso,
    Iso,
    Iso',
    LabelOptic (..),
    Lens',
    iso,
    over,
    prism,
    re,
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
-- @since 0.1
throwLeft :: forall e a. (Exception e) => Either e a -> IO a
throwLeft = either throwCS pure
{-# INLINEABLE throwLeft #-}

-- | @throwMaybe e x@ throws @e@ if @x@ is 'Nothing'.
--
-- @since 0.1
throwMaybe :: forall e a. (Exception e) => e -> Maybe a -> IO a
throwMaybe e = maybe (throwCS e) pure
{-# INLINEABLE throwMaybe #-}

-- | 'Text' version of 'show'.
--
-- @since 0.1
showt :: (Show a) => a -> Text
showt = T.pack . show
{-# INLINEABLE showt #-}

-- | @since 0.1
natToDouble :: Natural -> Double
natToDouble = fromIntegral
{-# INLINE natToDouble #-}
