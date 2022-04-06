{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the 'RunApp' type.
--
-- @since 0.1.0.0
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
-- @since 0.1.0.0
data RunApp a
  = -- | Do not specify an application. Attempt to run all, returning
    -- the first success.
    --
    -- @since 0.1.0.0
    Many
  | -- | Runs a single query based on the parameter app.
    --
    -- @since 0.1.0.0
    Single a
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
makePrismLabels ''RunApp

-- | @since 0.1.0.0
instance Semigroup a => Semigroup (RunApp a) where
  Many <> r = r
  l <> Many = l
  Single l <> Single r = Single (l <> r)

-- | @since 0.1.0.0
instance Semigroup a => Monoid (RunApp a) where
  mempty = Many
