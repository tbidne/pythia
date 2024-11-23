{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Pythia.Runner.Command.NetInterface
  ( -- * App
    parseNetInterfaceApp,

    -- * Field
    NetInterfaceField (..),
    parseNetInterfaceField,

    -- * Device
    NetInterfaceDevice (..),
    parseNetInterfaceDevice,

    -- * Toml
    NetInterfaceToml (..),
    fieldKey,
    deviceKey,

    -- * Handler
    handleNetInterface,
  )
where

import Pythia
  ( Device (MkDevice),
    NetInterface,
    NetInterfaceApp (NetInterfaceAppIp, NetInterfaceAppNmCli),
    NetInterfaces (unNetInterfaces),
    queryNetInterface,
    queryNetInterfaces,
  )
import Pythia.Prelude
import Pythia.Runner.Utils qualified as Utils
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- | @since 0.1
parseNetInterfaceApp :: (MonadFail m) => m Text -> m NetInterfaceApp
parseNetInterfaceApp =
  Utils.decodeKeyValPairs
    [ ("ip", NetInterfaceAppIp),
      ("nmcli", NetInterfaceAppNmCli)
    ]

-- | Extra option for NetInterfaceCmd.
--
-- @since 0.1
type NetInterfaceField :: Type
data NetInterfaceField
  = -- | @since 0.1
    NetInterfaceFieldDefault
  | -- | @since 0.1
    NetInterfaceFieldName
  | -- | @since 0.1
    NetInterfaceFieldIpv4
  | -- | @since 0.1
    NetInterfaceFieldIpv6
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance DecodeTOML NetInterfaceField where
  tomlDecoder = parseNetInterfaceField tomlDecoder

-- | @since 0.1
parseNetInterfaceField :: (MonadFail m) => m Text -> m NetInterfaceField
parseNetInterfaceField =
  Utils.decodeKeyValPairs
    [ ("default", NetInterfaceFieldDefault),
      ("name", NetInterfaceFieldName),
      ("ipv4", NetInterfaceFieldIpv4),
      ("ipv6", NetInterfaceFieldIpv6)
    ]

-- | @since 0.1
type NetInterfaceDevice :: Type
data NetInterfaceDevice
  = -- | @since 0.1
    NetInterfaceDeviceNone
  | -- | @since 0.1
    NetInterfaceDevice Device
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance IsString NetInterfaceDevice where
  fromString = NetInterfaceDevice . fromString

-- | @since 0.1
instance DecodeTOML NetInterfaceDevice where
  tomlDecoder = parseNetInterfaceDevice tomlDecoder

-- | @since 0.1
parseNetInterfaceDevice :: (MonadFail m) => m Text -> m NetInterfaceDevice
parseNetInterfaceDevice =
  Utils.decodeKeyValPairsDefault
    (NetInterfaceDevice . MkDevice)
    [ ("none", NetInterfaceDeviceNone)
    ]

-- | @since 0.1
data NetInterfaceToml = MkNetInterfaceToml
  { -- | @since 0.1
    app :: Maybe NetInterfaceApp,
    -- | @since 0.1
    field :: Maybe NetInterfaceField,
    -- | @since 0.1
    device :: Maybe NetInterfaceDevice
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
  LabelOptic "app" k NetInterfaceToml NetInterfaceToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfaceToml _app _field _device) ->
        fmap
          (\app' -> MkNetInterfaceToml app' _field _device)
          (f _app)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NetInterfaceField, b ~ Maybe NetInterfaceField) =>
  LabelOptic "field" k NetInterfaceToml NetInterfaceToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfaceToml _app _field _device) ->
        fmap
          (\field' -> MkNetInterfaceToml _app field' _device)
          (f _field)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NetInterfaceDevice, b ~ Maybe NetInterfaceDevice) =>
  LabelOptic "device" k NetInterfaceToml NetInterfaceToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkNetInterfaceToml _app _field _device) ->
        fmap
          (MkNetInterfaceToml _app _field)
          (f _device)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML NetInterfaceToml where
  tomlDecoder =
    MkNetInterfaceToml
      <$> getFieldOptWith (parseNetInterfaceApp tomlDecoder) "app"
      <*> getFieldOptWith tomlDecoder fieldKey
      <*> getFieldOptWith tomlDecoder deviceKey

-- | @since 0.1
fieldKey :: (IsString s) => s
fieldKey = "field"

-- | @since 0.1
deviceKey :: (IsString s) => s
deviceKey = "device"

-- | @since 0.1
handleNetInterface ::
  ( PathReader :> es,
    Terminal :> es,
    TypedProcess :> es
  ) =>
  NetInterfaceApp ->
  NetInterfaceDevice ->
  NetInterfaceField ->
  Eff es ()
handleNetInterface cfg mdevice field = do
  resultTxt <- case mdevice of
    NetInterfaceDeviceNone -> interfacesToText <$> Pythia.queryNetInterfaces cfg
    NetInterfaceDevice device -> interfaceToText <$> Pythia.queryNetInterface device cfg
  putTextLn resultTxt
  where
    interfacesToText :: NetInterfaces -> Text
    interfacesToText =
      case field of
        -- special case so we can use NetInterface's Pretty instance directly,
        -- which will print extra newlines between interfaces.
        NetInterfaceFieldDefault -> display
        _ ->
          builderToText
            . vsep
            . fmap (toField field)
            . view #unNetInterfaces

    interfaceToText :: NetInterface -> Text
    interfaceToText = builderToText . toField field

    toField :: NetInterfaceField -> NetInterface -> Builder
    toField NetInterfaceFieldDefault = displayBuilder
    toField NetInterfaceFieldName = displayMaybe . view #name
    toField NetInterfaceFieldIpv4 = displayBuilder . view #ipv4s
    toField NetInterfaceFieldIpv6 = displayBuilder . view #ipv6s
