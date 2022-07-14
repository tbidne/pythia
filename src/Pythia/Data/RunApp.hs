{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the 'RunApp' type.
--
-- @since 0.1
module Pythia.Data.RunApp
  ( RunApp (..),
    _RunAppMany,
    _RunAppSingle,
  )
where

import Pythia.Prelude

-- | Used in conjunction for our services for indicating how we want to run
-- queries. 'RunAppMany' is the identity for its monoid instance.
--
-- ==== __Examples__
--
-- >>> RunAppMany <> RunAppSingle "r"
-- RunAppSingle "r"
--
-- >>> RunAppSingle "l" <> RunAppMany
-- RunAppSingle "l"
--
-- >>> mempty @(RunApp String)
-- RunAppMany
--
-- @since 0.1
type RunApp :: Type -> Type
data RunApp a
  = -- | Do not specify an application. Attempt to run all, returning
    -- the first success.
    --
    -- @since 0.1
    RunAppMany
  | -- | Runs a single query based on the parameter app.
    --
    -- @since 0.1
    RunAppSingle a
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
  RunAppMany <> r = r
  l <> RunAppMany = l
  RunAppSingle l <> RunAppSingle r = RunAppSingle (l <> r)
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Semigroup a => Monoid (RunApp a) where
  mempty = RunAppMany
  {-# INLINEABLE mempty #-}
