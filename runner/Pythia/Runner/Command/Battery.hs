{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Pythia.Runner.Command.Battery
  ( -- * App
    parseBatteryApp,

    -- * Field
    BatteryField (..),
    parseBatteryField,

    -- * Toml
    BatteryToml (..),
    fieldKey,

    -- * Handler
    handleBattery,
  )
where

import Pythia
  ( BatteryApp (BatteryAppAcpi, BatteryAppSysFs, BatteryAppUPower),
    queryBattery,
  )
import Pythia.Prelude
import Pythia.Runner.Utils qualified as Utils
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- | Extra option for BatteryCmd.
--
-- @since 0.1
type BatteryField :: Type
data BatteryField
  = BatteryFieldDefault
  | BatteryFieldPercentage
  | BatteryFieldStatus
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance DecodeTOML BatteryField where
  tomlDecoder = parseBatteryField tomlDecoder

-- | @since 0.1
parseBatteryApp :: (MonadFail m) => m Text -> m BatteryApp
parseBatteryApp =
  Utils.decodeKeyValPairs
    [ ("acpi", BatteryAppAcpi),
      ("sysfs", BatteryAppSysFs),
      ("upower", BatteryAppUPower)
    ]

-- | @since 0.1
parseBatteryField :: (MonadFail m) => m Text -> m BatteryField
parseBatteryField =
  Utils.decodeKeyValPairs
    [ ("default", BatteryFieldDefault),
      ("percentage", BatteryFieldPercentage),
      ("status", BatteryFieldStatus)
    ]

-- | @since 0.1
data BatteryToml = MkBatteryToml
  { -- | @since 0.1
    app :: Maybe BatteryApp,
    -- | @since 0.1
    field :: Maybe BatteryField
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe BatteryApp, b ~ Maybe BatteryApp) =>
  LabelOptic "app" k BatteryToml BatteryToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkBatteryToml _app _field) ->
        fmap
          (`MkBatteryToml` _field)
          (f _app)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe BatteryField, b ~ Maybe BatteryField) =>
  LabelOptic "field" k BatteryToml BatteryToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkBatteryToml _app _field) ->
        fmap
          (MkBatteryToml _app)
          (f _field)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML BatteryToml where
  tomlDecoder =
    MkBatteryToml
      <$> getFieldOptWith (parseBatteryApp tomlDecoder) "app"
      <*> getFieldOptWith tomlDecoder fieldKey

-- | @since 0.1
fieldKey :: (IsString s) => s
fieldKey = "field"

-- | @since 0.1
handleBattery ::
  ( MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadTypedProcess m
  ) =>
  (Text -> m a) ->
  BatteryApp ->
  BatteryField ->
  m a
handleBattery handler cfg field =
  queryBattery cfg
    >>= handler
    . toField field
  where
    toField BatteryFieldDefault = display
    toField BatteryFieldPercentage = display . view #percentage
    toField BatteryFieldStatus = display . view #status
