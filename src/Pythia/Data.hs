{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides common data types.
--
-- @since 0.1.0.0
module Pythia.Data
  ( Command (..),
    QueryError (..),
    QueryResult,
    refineExToQueryError,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Optics.TH qualified as OTH
import Refined (RefineException (..))

-- | Newtype wrapper over a shell command.
--
-- @since 0.1.0.0
newtype Command = MkCommand
  { -- | @since 0.1.0.0
    unCommand :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      IsString
    )
    via Text

OTH.makeFieldLabelsNoPrefix ''Command

-- | Core error type.
--
-- @since 0.1.0.0
data QueryError = MkQueryError
  { -- | The name associated to the error (usually a module name).
    --
    -- @since 0.1.0.0
    name :: Text,
    -- | A short description of the error.
    --
    -- @since 0.1.0.0
    short :: Text,
    -- | More detailed description.
    --
    -- @since 0.1.0.0
    long :: Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

OTH.makeFieldLabelsNoPrefix ''QueryError

-- | Return type for running a command/query.
--
-- @since 0.1.0.0
type QueryResult result = Either [QueryError] result

-- | Maps a 'RefineException' to a 'QueryError'.
--
-- @since 0.1.0.0
refineExToQueryError :: RefineException -> QueryError
refineExToQueryError (MkRefineException predRep targetRep msg) = qe
  where
    shortErr =
      "Error when refining from type "
        <> show targetRep
        <> " to type "
        <> show predRep
    qe =
      MkQueryError
        { name = "Refinement Error",
          short = T.pack shortErr,
          long = T.pack msg
        }
