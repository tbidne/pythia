-- | This module exports time related services.
--
-- @since 0.1
module Pythia.Services.Time
  ( -- * Queries
    queryLocalTime,
    queryUTC,
    queryTimeZone,
    queryTimeZoneLabel,

    -- * Types
    ZonedTime (..),
    UTCTime (..),
    TZLabel (..),
  )
where

import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock qualified as Clock
import Data.Time.Conversion (TZDatabase (..), TZLabel (..), ZonedTime (..))
import Data.Time.Conversion qualified as TimeConv
import Pythia.Prelude

-- | Queries current local time.
--
-- @since 0.1
queryLocalTime :: IO ZonedTime
queryLocalTime = TimeConv.readTime Nothing
{-# INLINEABLE queryLocalTime #-}

-- | Queries current UTC time.
--
-- @since 0.1
queryUTC :: IO UTCTime
queryUTC = Clock.getCurrentTime
{-# INLINEABLE queryUTC #-}

-- | Queries current time in the given timezone.
--
-- @
-- queryTimeZone "America/New_York"
-- @
--
-- @since 0.1
queryTimeZone :: Text -> IO ZonedTime
queryTimeZone =
  TimeConv.readConvertTime Nothing
    . Just
    . TZDatabaseText
{-# INLINEABLE queryTimeZone #-}

-- | Queries current time in the given timezone.
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryTimeZoneLabel :: TZLabel -> IO ZonedTime
queryTimeZoneLabel =
  TimeConv.readConvertTime Nothing
    . Just
    . TZDatabaseLabel
{-# INLINEABLE queryTimeZoneLabel #-}
