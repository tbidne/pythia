-- | @since 0.1
module Pythia.Runner.These
  ( These (..),
  )
where

import Pythia.Prelude

-- | So we don't have to add package @these@ as a dependency.
--
-- @since 0.1
type These :: Type -> Type -> Type
data These a b
  = -- | @since 0.1
    This a
  | -- | @since 0.1
    That b
  | -- | @since 0.1
    These a b
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
