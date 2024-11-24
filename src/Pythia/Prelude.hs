{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

    -- * Display Utils
    (<+>),
    hsep,
    vsep,
    punctuate,
    comma,
    line,
    displayMaybe,
    displayList,
    builderToText,

    -- * Base
    module X,
  )
where

import Control.Applicative as X
  ( Alternative (empty, (<|>)),
    Applicative (pure, (*>), (<*), (<*>)),
  )
import Control.DeepSeq as X (NFData)
import Control.Exception as X
  ( Exception
      ( displayException,
        fromException,
        toException
      ),
    SomeException,
  )
import Control.Exception.Utils as X (trySync)
import Control.Monad as X
  ( Monad ((>>=)),
    join,
    unless,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Catch as X (MonadCatch, MonadThrow, throwM)
import Control.Monad.Fail as X (MonadFail (fail))
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Char as X (Char)
import Data.Either as X (Either (Left, Right), either)
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X
  ( Foldable (foldMap, foldl', foldr, length, null),
    foldr1,
    for_,
  )
import Data.Function as X (const, id, ($), (.))
import Data.Functor as X (Functor (fmap), ($>), (<$>), (<&>))
import Data.Int as X (Int)
import Data.Kind as X (Type)
import Data.List as X (filter, replicate)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X (Ord ((<=), (>=)), (<), (>))
import Data.Proxy as X (Proxy (Proxy))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.Display as X (Display (displayBuilder), display)
import Data.Traversable as X (Traversable (traverse), for)
import Data.Tuple as X (uncurry)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality as X (type (~))
#endif
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder as X (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import Data.Void as X (Void)
import Data.Word as X (Word8)
import Effects.FileSystem.FileReader as X
  ( MonadFileReader,
    decodeUtf8Lenient,
    readFileUtf8Lenient,
  )
import Effects.FileSystem.PathReader as X (MonadPathReader)
import Effects.Process.Typed as X (MonadTypedProcess)
import Effects.System.Terminal as X (MonadTerminal (putStrLn), print, putTextLn)
import Effects.Time as X (MonadTime)
import FileSystem.OsPath as X (OsPath, decodeLenient, osp, (</>))
import GHC.Enum as X (Bounded (maxBound, minBound), Enum (toEnum))
import GHC.Err as X (error, undefined)
import GHC.Float as X (Double, Float)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num ((*), (+), (-)))
import GHC.Read as X (Read)
import GHC.Real as X (even, floor, fromIntegral, (/))
import GHC.Show as X (Show (show))
import GHC.Stack as X (HasCallStack)
import Optics.Core as X
  ( A_Lens,
    A_Prism,
    AffineTraversal',
    An_Iso,
    Iso,
    Iso',
    LabelOptic (labelOptic),
    Lens',
    Optic,
    Prism',
    iso,
    lensVL,
    over,
    preview,
    prism,
    re,
    view,
    (%),
    (%?),
    (%~),
    (.~),
    (^.),
    (^?),
    _1,
    _2,
    _Just,
    _Left,
    _Nothing,
    _Right,
  )
import System.IO as X (FilePath, IO)

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
throwLeft ::
  forall m e a.
  ( Exception e,
    HasCallStack,
    MonadThrow m
  ) =>
  Either e a ->
  m a
throwLeft = either throwM pure
{-# INLINEABLE throwLeft #-}

-- | @throwMaybe e x@ throws @e@ if @x@ is 'Nothing'.
--
-- @since 0.1
throwMaybe ::
  forall m e a.
  ( Exception e,
    HasCallStack,
    MonadThrow m
  ) =>
  e ->
  Maybe a ->
  m a
throwMaybe e = maybe (throwM e) pure
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

hsep :: [Builder] -> Builder
hsep = concatWith (<+>)

vsep :: [Builder] -> Builder
vsep = concatWith (\x y -> x <> line <> y)

punctuate :: Builder -> [Builder] -> [Builder]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate p (x : xs) = x <> p : punctuate p xs

(<+>) :: Builder -> Builder -> Builder
x <+> y = x <> " " <> y

comma :: Builder
comma = ","

line :: Builder
line = "\n"

-- vendored from prettyprinter, for text's Builder
concatWith :: (Foldable t) => (Builder -> Builder -> Builder) -> t Builder -> Builder
concatWith f ds
  | null ds = mempty
  | otherwise = foldr1 f ds

displayMaybe :: (Display a) => Maybe a -> Builder
displayMaybe (Just x) = displayBuilder x
displayMaybe Nothing = "<nothing>"

displayList :: (Display a) => [a] -> Builder
displayList [] = "<empty>"
displayList xs@(_ : _) =
  hsep
    . punctuate comma
    . fmap displayBuilder
    $ xs

-- | Intermediate function for Display a => a -> Text, for when we want to
-- use a custom builder, and not the type's built-in displayBuilder.
builderToText :: Builder -> Text
builderToText = TL.toStrict . TLB.toLazyText
