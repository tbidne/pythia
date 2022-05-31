{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

    -- ** Errors
    TimeException (..),
  )
where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock qualified as Clock
import Data.Time.LocalTime (LocalTime, TimeZone, ZonedTime (..))
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones (TZ)
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Prelude
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U

-- $setup
-- >>> import GHC.Exception (errorCallException)

-- | Errors that can occur with time.
--
-- ==== __Examples__
--
-- >>> putStrLn $ displayException $ TimeGeneralException $ errorCallException "oh no"
-- Time exception: <oh no>
--
-- >>> putStrLn $ displayException $ TimeZoneParseException "bad_tz"
-- Time parse timezone exception: <bad_tz>
--
-- @since 0.1
type TimeException :: Type
data TimeException
  = -- | For general exceptions.
    --
    -- @since 0.1
    forall e. Exception e => TimeGeneralException e
  | -- | Parse errors.
    --
    -- @since 0.1
    TimeZoneParseException Text

-- | @since 0.1
makePrismLabels ''TimeException

-- | @since 0.1
deriving stock instance Show TimeException

-- | @since 0.1
instance Pretty TimeException where
  pretty (TimeGeneralException e) =
    pretty @Text "Time exception: <"
      <> pretty (displayException e)
      <> pretty @Text ">"
  pretty (TimeZoneParseException s) =
    pretty @Text "Time parse timezone exception: <"
      <> pretty s
      <> pretty @Text ">"
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception TimeException where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | Queries current local time.
--
-- @since 0.1
queryLocalTime :: IO ZonedTime
queryLocalTime = Local.getZonedTime `catchAny` (throwIO . TimeGeneralException)
{-# INLINEABLE queryLocalTime #-}

-- | Queries current UTC time.
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryUTC :: IO UTCTime
queryUTC = Clock.getCurrentTime `catchAny` (throwIO . TimeGeneralException)
{-# INLINEABLE queryUTC #-}

-- | Queries current time in the given timezone.
--
-- @
-- queryTimeZone "America/New_York"
-- @
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryTimeZone :: Text -> IO ZonedTime
queryTimeZone tzStr = do
  let bs = TEnc.encodeUtf8 tzStr
  case All.fromTZName bs of
    Just label -> queryTimeZoneLabel label
    Nothing -> throwIO $ TimeZoneParseException tzStr
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
queryTimeZoneLabel tzLabel =
  (\x -> ZonedTime (toLT x) (toTZ x))
    <$> getTZandCurrUTC `catchAny` (throwIO . TimeGeneralException)
  where
    getTZandCurrUTC = (All.tzByLabel tzLabel,) <$> queryUTC
{-# INLINEABLE queryTimeZoneLabel #-}

toTZ :: (TZ, UTCTime) -> TimeZone
toTZ = uncurry Zones.timeZoneForUTCTime
{-# INLINE toTZ #-}

toLT :: (TZ, UTCTime) -> LocalTime
toLT = uncurry Zones.utcToLocalTimeTZ
{-# INLINE toLT #-}
