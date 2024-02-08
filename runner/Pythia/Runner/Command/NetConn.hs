{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Pythia.Runner.Command.NetConn
  ( -- * Field
    NetConnField (..),
    parseNetConnField,

    -- * Toml
    NetConnToml (..),
    fieldKey,

    -- * Handler
    handleNetConn,
  )
where

import Pythia
  ( NetInterface,
    NetInterfaceApp,
    findUp,
    queryNetInterfaces,
  )
import Pythia.Prelude
import Pythia.Runner.Command.NetInterface (parseNetInterfaceApp)
import Pythia.Runner.Utils qualified as Utils
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- | Extra option for NetConnCmd.
--
-- @since 0.1
type NetConnField :: Type
data NetConnField
  = -- | @since 0.1
    NetConnFieldDefault
  | -- | @since 0.1
    NetConnFieldDevice
  | -- | @since 0.1
    NetConnFieldType
  | -- | @since 0.1
    NetConnFieldName
  | -- | @since 0.1
    NetConnFieldIpv4
  | -- | @since 0.1
    NetConnFieldIpv6
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance DecodeTOML NetConnField where
  tomlDecoder = parseNetConnField tomlDecoder

-- | @since 0.1
parseNetConnField :: (MonadFail m) => m Text -> m NetConnField
parseNetConnField =
  Utils.decodeKeyValPairs
    [ ("default", NetConnFieldDefault),
      ("device", NetConnFieldDevice),
      ("type", NetConnFieldType),
      ("name", NetConnFieldName),
      ("ipv4", NetConnFieldIpv4),
      ("ipv6", NetConnFieldIpv6)
    ]

-- | @since 0.1
data NetConnToml = MkNetConnToml
  { -- | @since 0.1
    app :: Maybe NetInterfaceApp,
    -- | @since 0.1
    field :: Maybe NetConnField
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NetInterfaceApp, b ~ Maybe NetInterfaceApp) =>
  LabelOptic "app" k NetConnToml NetConnToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetConnToml _app _field) ->
        fmap
          (`MkNetConnToml` _field)
          (f _app)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NetConnField, b ~ Maybe NetConnField) =>
  LabelOptic "field" k NetConnToml NetConnToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetConnToml _app _field) ->
        fmap
          (MkNetConnToml _app)
          (f _field)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML NetConnToml where
  tomlDecoder =
    MkNetConnToml
      <$> getFieldOptWith (parseNetInterfaceApp tomlDecoder) "app"
      <*> getFieldOptWith tomlDecoder "field"

-- | @since 0.1
fieldKey :: (IsString s) => s
fieldKey = "field"

-- | @since 0.1
handleNetConn ::
  ( MonadPathReader m,
    MonadTerminal m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  NetInterfaceApp ->
  NetConnField ->
  m ()
handleNetConn cfg field = do
  result <- Pythia.queryNetInterfaces cfg
  putTextLn $ case Pythia.findUp result of
    Nothing -> "<No live connection found>"
    Just conn -> toField field conn
  where
    toField :: NetConnField -> NetInterface -> Text
    toField NetConnFieldDefault = display
    toField NetConnFieldDevice = display . view #device
    toField NetConnFieldType = builderToText . displayMaybe . view #ntype
    toField NetConnFieldName = builderToText . displayMaybe . view #name
    toField NetConnFieldIpv4 = display . view #ipv4s
    toField NetConnFieldIpv6 = display . view #ipv6s
