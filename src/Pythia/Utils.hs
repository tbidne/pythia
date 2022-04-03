-- | This module provides common utilities.
--
-- @since 0.1.0.0
module Pythia.Utils
  ( -- * Folding
    foldAlt,
    mAlt,

    -- * Parsing
    takeLine,
    takeLineLabel,
    takeLine_,
    exeSupported,

    -- * Exceptions
    uncheck2,
    uncheck3,
    uncheck4,
    uncheck5,
    uncheck6,
    -- $rethrow
    rethrow2,
    rethrow3,
    rethrow4,
    rethrow5,
    rethrow6,

    -- * Miscellaneous
    headMaybe,
    eitherToBool,
  )
where

import Data.Maybe qualified as May
import Pythia.Prelude
import System.Directory qualified as Dir
import Text.Megaparsec (Parsec, Stream, Token, Tokens)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Megaparsec (parseTest)

-- | Safe version of 'Prelude.head'.
--
-- @since 0.1.0.0
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Similar to 'foldMap' but for 'Alternative'.
--
-- ==== __Examples__
-- >>> foldAlt (\c -> if even c then Just c else Nothing) [1,2,3,4]
-- Just 2
--
-- >>> foldAlt (\c -> if even c then Just c else Nothing) [1,3]
-- Nothing
--
-- @since 0.1.0.0
foldAlt :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldAlt f = foldr ((<|>) . f) empty

-- | Convenience function for mapping a 'Maybe' to its underlying
-- 'Alternative'.
--
-- ==== __Examples__
-- >>> mAlt @[] Nothing
-- []
--
-- >>> mAlt @[] (Just [1,2,3])
-- [1,2,3]
--
-- @since 0.1.0.0
mAlt :: Alternative f => Maybe (f a) -> f a
mAlt = fromMaybe empty

-- | 'takeLineLabel' with no label.
--
-- ==== __Examples__
-- >>> parseTest @Void takeLine "some text 123 \n"
-- "some text 123 "
--
-- >>> parseTest @Void takeLine "some text 123"
-- 1:14:
--   |
-- 1 | some text 123
--   |              ^
-- unexpected end of input
-- expecting end of line
--
-- @since 0.1.0.0
takeLine :: (Ord e, Stream s, Token s ~ Char) => Parsec e s (Tokens s)
takeLine = takeLineLabel Nothing

-- | Variant of 'takeLine' taking in a label.
--
-- ==== __Examples__
-- >>> parseTest @Void (takeLineLabel (Just "a label")) "some text 123"
-- 1:14:
--   |
-- 1 | some text 123
--   |              ^
-- unexpected end of input
-- expecting a label or end of line
--
-- @since 0.1.0.0
takeLineLabel :: (Ord e, Stream s, Token s ~ Char) => Maybe String -> Parsec e s (Tokens s)
takeLineLabel desc = MP.takeWhileP desc (/= '\n') <* MPC.eol

-- | Takes everything up to the first new line, returns unit.
--
-- ==== __Examples__
-- >>> parseTest @Void takeLine_ "some text 123\n"
-- ()
--
-- @since 0.1.0.0
takeLine_ :: (Ord e, Stream s, Token s ~ Char) => Parsec e s ()
takeLine_ = MP.takeWhileP Nothing (/= '\n') *> void MPC.eol

-- | Maps 'Left' to 'False', 'Right' to 'True'.
--
-- ==== __Examples__
-- >>> eitherToBool (Left ())
-- False
--
-- >>> eitherToBool (Right ())
-- True
--
-- @since 0.1.0.0
eitherToBool :: Either a b -> Bool
eitherToBool = either (const False) (const True)

-- | Determines if the executable represented by the string parameter is
-- supported on this system.
--
-- @since 0.1.0.0
exeSupported :: MonadIO m => String -> m Bool
exeSupported exeName = liftIO $ May.isJust <$> Dir.findExecutable exeName

-- | Uncheck 2 checked exceptions.
--
-- @since 0.1.0.0
uncheck2 :: forall e f a. ((Throws e, Throws f) => a) -> a
uncheck2 x = uncheck (Proxy @e) $ uncheck (Proxy @f) x

-- | Uncheck 3 checked exceptions.
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

-- | Uncheck 6 checked exceptions.
--
-- @since 0.1.0.0
uncheck6 ::
  forall e f g h i j a.
  ( ( Throws e,
      Throws f,
      Throws g,
      Throws h,
      Throws i,
      Throws j
    ) =>
    a
  ) ->
  a
uncheck6 x = uncheck (Proxy @e) $ uncheck5 @f @g @h @i @j x

-- $rethrow
-- Given a polymorphic "lift error" @f :: forall x b. Either x b -> m b@,
-- we can lift functions that throw checked exceptions into ones that do not.
-- This is useful when we want to "transform" some kind of exception e.g.
-- for the purpose of catching several and rethrowing as another type.

-- | Lifts 2 exceptions into 'MonadCatch' @m@.
--
-- @since 0.1.0.0
rethrow2 ::
  forall e f m a.
  ( Exception e,
    Exception f,
    MonadCatch m
  ) =>
  (forall x b. Either x b -> m b) ->
  ( ( Throws e,
      Throws f
    ) =>
    m a
  ) ->
  m a
rethrow2 f mx = do
  x <-
    try @_ @e $
      try @_ @f mx
  g x
  where
    g = f >=> f

-- | Lifts 3 exceptions into 'MonadCatch' @m@.
--
-- @since 0.1.0.0
rethrow3 ::
  forall e f g m a.
  ( Exception e,
    Exception f,
    Exception g,
    MonadCatch m
  ) =>
  (forall x b. Either x b -> m b) ->
  ( ( Throws e,
      Throws f,
      Throws g
    ) =>
    m a
  ) ->
  m a
rethrow3 f mx = f =<< try @_ @e (rethrow2 @f @g f mx)

-- | Lifts 4 exceptions into 'MonadCatch' @m@.
--
-- @since 0.1.0.0
rethrow4 ::
  forall e f g h m a.
  ( Exception e,
    Exception f,
    Exception g,
    Exception h,
    MonadCatch m
  ) =>
  (forall x b. Either x b -> m b) ->
  ( ( Throws e,
      Throws f,
      Throws g,
      Throws h
    ) =>
    m a
  ) ->
  m a
rethrow4 f mx = try @_ @e (rethrow3 @f @g @h f mx) >>= f

-- | Lifts 5 exceptions into 'MonadCatch' @m@.
--
-- @since 0.1.0.0
rethrow5 ::
  forall e f g h i m a.
  ( Exception e,
    Exception f,
    Exception g,
    Exception h,
    Exception i,
    MonadCatch m
  ) =>
  (forall x b. Either x b -> m b) ->
  ( ( Throws e,
      Throws f,
      Throws g,
      Throws h,
      Throws i
    ) =>
    m a
  ) ->
  m a
rethrow5 f mx = try @_ @e (rethrow4 @f @g @h @i f mx) >>= f

-- | Lifts 6 exceptions into 'MonadCatch' @m@.
--
-- @since 0.1.0.0
rethrow6 ::
  forall e f g h i j m a.
  ( Exception e,
    Exception f,
    Exception g,
    Exception h,
    Exception i,
    Exception j,
    MonadCatch m
  ) =>
  (forall x b. Either x b -> m b) ->
  ( ( Throws e,
      Throws f,
      Throws g,
      Throws h,
      Throws i,
      Throws j
    ) =>
    m a
  ) ->
  m a
rethrow6 f mx = try @_ @e (rethrow5 @f @g @h @i @j f mx) >>= f
