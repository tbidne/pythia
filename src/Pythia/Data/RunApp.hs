{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the 'RunApp' type.
--
-- @since 0.1
module Pythia.Data.RunApp
  ( RunApp (..),
  )
where

import Pythia.Prelude

-- | Used in conjunction for our services for indicating how we want to run
-- queries. 'Many' is the identity for its monoid instance.
--
-- ==== __Examples__
--
-- >>> Many <> Single "r"
-- Single "r"
--
-- >>> Single "l" <> Many
-- Single "l"
--
-- >>> mempty @(RunApp String)
-- Many
--
-- @since 0.1
type RunApp :: Type -> Type
data RunApp a
  = -- | Do not specify an application. Attempt to run all, returning
    -- the first success.
    --
    -- @since 0.1
    Many
  | -- | Runs a single query based on the parameter app.
    --
    -- @since 0.1
    Single a
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
makePrisms ''RunApp

-- | @since 0.1
instance Semigroup a => Semigroup (RunApp a) where
  Many <> r = r
  l <> Many = l
  Single l <> Single r = Single (l <> r)
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Semigroup a => Monoid (RunApp a) where
  mempty = Many
  {-# INLINEABLE mempty #-}
