{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Pythia.Runner.Command.Time
  ( -- * Dest
    TimezoneDest (..),
    parseTimeDest,

    -- * Format
    TimeFormat (..),
    parseTimeFormat,

    -- * Toml
    TimeToml (..),
    destKey,
    formatKey,

    -- * Handler
    handleTime,
  )
where

import Data.Text qualified as T
import Data.Time.Format (FormatTime)
import Data.Time.Format qualified as Format
import Pythia (queryLocalTime, queryTimeZone, queryUTC)
import Pythia.Prelude
import Pythia.Runner.Utils qualified as Utils
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- | Time type.
--
-- @since 0.1
type TimezoneDest :: Type
data TimezoneDest
  = -- | @since 0.1
    TimezoneDestLocal
  | -- | @since 0.1
    TimezoneDestUTC
  | -- | @since 0.1
    TimezoneDestTZ Text
  deriving stock (Eq, Show)

-- | @since 0.1
instance DecodeTOML TimezoneDest where
  tomlDecoder = parseTimeDest tomlDecoder

-- | @since 0.1
parseTimeDest :: (MonadFail m) => m Text -> m TimezoneDest
parseTimeDest =
  Utils.decodeKeyValPairsDefault
    TimezoneDestTZ
    [ ("local", TimezoneDestLocal),
      ("utc", TimezoneDestUTC)
    ]

-- | @since 0.1
data TimeFormat
  = TimeFormatDefault
  | TimeFormatCustom Text
  deriving stock (Eq, Show)

-- | @since 0.1
instance IsString TimeFormat where
  fromString = TimeFormatCustom . T.pack

-- | @since 0.1
instance DecodeTOML TimeFormat where
  tomlDecoder = parseTimeFormat tomlDecoder

-- | @since 0.1
parseTimeFormat :: (MonadFail m) => m Text -> m TimeFormat
parseTimeFormat =
  Utils.decodeKeyValPairsDefault
    TimeFormatCustom
    [ ("default", TimeFormatDefault)
    ]

-- | @since 0.1
data TimeToml = MkTimeToml
  { -- | @since 0.1
    dest :: Maybe TimezoneDest,
    -- | @since 0.1
    format :: Maybe TimeFormat
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe TimezoneDest, b ~ Maybe TimezoneDest) =>
  LabelOptic "dest" k TimeToml TimeToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkTimeToml _dest _format) ->
        fmap
          (`MkTimeToml` _format)
          (f _dest)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe TimeFormat, b ~ Maybe TimeFormat) =>
  LabelOptic "format" k TimeToml TimeToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkTimeToml _dest _format) ->
        fmap
          (MkTimeToml _dest)
          (f _format)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML TimeToml where
  tomlDecoder =
    MkTimeToml
      <$> getFieldOptWith tomlDecoder destKey
      <*> getFieldOptWith tomlDecoder formatKey

-- | @since 0.1
destKey :: (IsString s) => s
destKey = "dest"

-- | @since 0.1
formatKey :: (IsString s) => s
formatKey = "format"

-- | @since 0.1
handleTime ::
  ( Terminal :> es,
    Time :> es
  ) =>
  TimeFormat ->
  TimezoneDest ->
  Eff es ()
handleTime mformat = \case
  TimezoneDestLocal -> Pythia.queryLocalTime >>= putTextLn . formatTime
  TimezoneDestUTC -> Pythia.queryUTC >>= putTextLn . formatTime
  TimezoneDestTZ tz -> Pythia.queryTimeZone tz >>= putTextLn . formatTime
  where
    format = case mformat of
      TimeFormatDefault -> Format.rfc822DateFormat
      TimeFormatCustom fmt -> T.unpack fmt

    formatTime :: (FormatTime t) => t -> Text
    formatTime = T.pack . Format.formatTime Format.defaultTimeLocale format
