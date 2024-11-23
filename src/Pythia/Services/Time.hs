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
import Data.Time.Conversion
  ( TZDatabase
      ( TZDatabaseLabel,
        TZDatabaseText
      ),
    TZLabel (..),
    ZonedTime,
  )
import Data.Time.Conversion qualified as TimeConv
import Data.Time.LocalTime qualified as LT
import Effectful.Time.Dynamic (getSystemZonedTime)
import Pythia.Prelude

-- | Queries current local time.
--
-- @since 0.1
queryLocalTime :: (HasCallStack, Time :> es) => Eff es ZonedTime
queryLocalTime = TimeConv.readTime Nothing

-- | Queries current UTC time.
--
-- @since 0.1
queryUTC :: (HasCallStack, Time :> es) => Eff es UTCTime
queryUTC = LT.zonedTimeToUTC <$> getSystemZonedTime

-- | Queries current time in the given timezone.
--
-- @
-- queryTimeZone "America/New_York"
-- @
--
-- @since 0.1
queryTimeZone :: (HasCallStack, Time :> es) => Text -> Eff es ZonedTime
queryTimeZone =
  TimeConv.readConvertTime Nothing
    . Just
    . TZDatabaseText

-- | Queries current time in the given timezone.
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryTimeZoneLabel :: (HasCallStack, Time :> es) => TZLabel -> Eff es ZonedTime
queryTimeZoneLabel =
  TimeConv.readConvertTime Nothing
    . Just
    . TZDatabaseLabel
