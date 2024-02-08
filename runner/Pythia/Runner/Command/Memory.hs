{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Pythia.Runner.Command.Memory
  ( -- * App
    parseMemoryApp,

    -- * Field
    MemoryField (..),
    parseMemoryField,

    -- * Units
    MemoryUnits (..),
    parseMemoryUnits,

    -- * Toml
    MemoryToml (..),
    fieldKey,
    unitsKey,

    -- * Handler
    handleMemory,
  )
where

import Data.Text qualified as T
import Pythia (MemoryApp (MemoryAppFree), SystemMemory)
import Pythia qualified
import Pythia.Prelude
import Pythia.Runner.Utils qualified as Utils
import Pythia.Services.Memory qualified as Mem
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- | Extra option for MemoryCmd.
--
-- @since 0.1
type MemoryField :: Type
data MemoryField
  = -- | @since 0.1
    MemoryFieldDefault
  | -- | @since 0.1
    MemoryFieldTotal
  | -- | @since 0.1
    MemoryFieldUsed
  | -- | @since 0.1
    MemoryFieldFree
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance DecodeTOML MemoryField where
  tomlDecoder = parseMemoryField tomlDecoder

-- | @since 0.1
parseMemoryApp :: (MonadFail m) => m Text -> m MemoryApp
parseMemoryApp =
  Utils.decodeKeyValPairs
    [ ("free", MemoryAppFree)
    ]

-- | @since 0.1
parseMemoryField :: (MonadFail m) => m Text -> m MemoryField
parseMemoryField =
  Utils.decodeKeyValPairs
    [ ("default", MemoryFieldDefault),
      ("total", MemoryFieldTotal),
      ("used", MemoryFieldUsed),
      ("free", MemoryFieldFree)
    ]

-- | Determines how to print memory.
--
-- @since 0.1
type MemoryUnits :: Type
data MemoryUnits
  = -- | @since 0.1
    MemoryUnitsBytes
  | -- | @since 0.1
    MemoryUnitsPercentage
  deriving stock (Eq, Show)

-- | @since 0.1
instance DecodeTOML MemoryUnits where
  tomlDecoder = parseMemoryUnits tomlDecoder

-- | @since 0.1
parseMemoryUnits :: (MonadFail m) => m Text -> m MemoryUnits
parseMemoryUnits =
  Utils.decodeKeyValPairs
    [ ("bytes", MemoryUnitsBytes),
      ("percentage", MemoryUnitsPercentage)
    ]

-- | @since 0.1
data MemoryToml = MkMemoryToml
  { -- | @since 0.1
    app :: Maybe MemoryApp,
    -- | @since 0.1
    field :: Maybe MemoryField,
    -- | @since 0.1
    units :: Maybe MemoryUnits
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe MemoryApp, b ~ Maybe MemoryApp) =>
  LabelOptic "app" k MemoryToml MemoryToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkMemoryToml _app _field _format) ->
        fmap
          (\app' -> MkMemoryToml app' _field _format)
          (f _app)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe MemoryField, b ~ Maybe MemoryField) =>
  LabelOptic "field" k MemoryToml MemoryToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkMemoryToml _app _field _format) ->
        fmap
          (\field' -> MkMemoryToml _app field' _format)
          (f _field)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe MemoryUnits, b ~ Maybe MemoryUnits) =>
  LabelOptic "units" k MemoryToml MemoryToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkMemoryToml _app _field _format) ->
        fmap
          (MkMemoryToml _app _field)
          (f _format)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML MemoryToml where
  tomlDecoder =
    MkMemoryToml
      <$> getFieldOptWith (parseMemoryApp tomlDecoder) "app"
      <*> getFieldOptWith tomlDecoder fieldKey
      <*> getFieldOptWith tomlDecoder unitsKey

-- | @since 0.1
fieldKey :: (IsString s) => s
fieldKey = "field"

-- | @since 0.1
unitsKey :: (IsString s) => s
unitsKey = "units"

-- | @since 0.1
handleMemory ::
  ( MonadPathReader m,
    MonadTerminal m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  MemoryApp ->
  MemoryField ->
  MemoryUnits ->
  m ()
handleMemory cfg field format =
  Pythia.queryMemory cfg
    >>= putTextLn
    . toField format field
  where
    toField :: MemoryUnits -> MemoryField -> SystemMemory -> Text
    toField MemoryUnitsBytes MemoryFieldDefault = display
    toField MemoryUnitsBytes MemoryFieldTotal = display . view #total
    toField MemoryUnitsBytes MemoryFieldUsed = display . view #used
    toField MemoryUnitsBytes MemoryFieldFree = display . Mem.freeMemory
    -- so we don't have an extra %
    toField MemoryUnitsPercentage MemoryFieldDefault =
      (<> " / 100%")
        . T.pack
        . show
        . view #unPercentage
        . Mem.percentageUsed
    toField MemoryUnitsPercentage MemoryFieldTotal = const "100%"
    toField MemoryUnitsPercentage MemoryFieldUsed = display . Mem.percentageUsed
    toField MemoryUnitsPercentage MemoryFieldFree = display . Mem.percentageFree
