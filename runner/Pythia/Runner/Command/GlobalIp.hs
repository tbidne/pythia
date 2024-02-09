{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Pythia.Runner.Command.GlobalIp
  ( -- * App
    parseGlobalIpApp,

    -- * Field
    GlobalIpField (..),
    parseGlobalIpField,

    -- * Toml
    GlobalIpToml (..),
    fieldKey,
    ipv4SrcKey,
    ipv6SrcKey,

    -- * Handler
    handleGlobalIp,
  )
where

import Pythia
  ( GlobalIpApp (GlobalIpAppCurl, GlobalIpAppDig),
    IpType (Ipv4, Ipv6),
    UrlSource (MkUrlSource),
  )
import Pythia qualified
import Pythia.Prelude
import Pythia.Runner.These (These (That, These, This))
import Pythia.Runner.Utils qualified as Utils
import TOML
  ( DecodeTOML (tomlDecoder),
    Decoder,
    getArrayOf,
    getFieldOptWith,
  )

-- | @since 0.1
parseGlobalIpApp :: (MonadFail m) => m Text -> m GlobalIpApp
parseGlobalIpApp =
  Utils.decodeKeyValPairs
    [ ("curl", GlobalIpAppCurl),
      ("dig", GlobalIpAppDig)
    ]

-- | Extra option for GlobalIpCmd.
--
-- @since 0.1
type GlobalIpField :: Type
data GlobalIpField
  = -- | @since 0.1
    GlobalIpFieldIpv4
  | -- | @since 0.1
    GlobalIpFieldIpv6
  | -- | @since 0.1
    GlobalIpFieldBoth
  deriving stock (Eq, Show)

-- | @since 0.1
instance DecodeTOML GlobalIpField where
  tomlDecoder = parseGlobalIpField tomlDecoder

-- | @since 0.1
parseGlobalIpField :: (MonadFail m) => m Text -> m GlobalIpField
parseGlobalIpField =
  Utils.decodeKeyValPairs
    [ ("ipv4", GlobalIpFieldIpv4),
      ("ipv6", GlobalIpFieldIpv6),
      ("both", GlobalIpFieldBoth)
    ]

-- | @since 0.1
handleGlobalIp ::
  ( MonadCatch m,
    MonadTerminal m,
    MonadTypedProcess m
  ) =>
  GlobalIpApp ->
  These [UrlSource Ipv4] [UrlSource Ipv6] ->
  m ()
handleGlobalIp app sources = do
  case sources of
    This ipv4Sources ->
      Pythia.queryGlobalIpv4 app ipv4Sources >>= prettyPrint
    That ipv6Sources ->
      Pythia.queryGlobalIpv6 app ipv6Sources >>= prettyPrint
    These ipv4Sources ipv6Sources -> do
      (ipv4Address, ipv6Address) <-
        Pythia.queryGlobalIp app ipv4Sources ipv6Sources

      prettyPrint ipv4Address
      prettyPrint ipv6Address

prettyPrint :: (Display a, MonadTerminal m) => a -> m ()
prettyPrint = putTextLn . display

-- | @since 0.1
data GlobalIpToml = MkGlobalIpToml
  { -- | @since 0.1
    app :: Maybe GlobalIpApp,
    -- | @since 0.1
    field :: Maybe GlobalIpField,
    -- | @since 0.1
    ipv4Sources :: [UrlSource Ipv4],
    -- | @since 0.1
    ipv6Sources :: [UrlSource Ipv6]
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe GlobalIpApp, b ~ Maybe GlobalIpApp) =>
  LabelOptic "app" k GlobalIpToml GlobalIpToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkGlobalIpToml _app _field _ipv4Sources _ipv6Sources) ->
        fmap
          (\app' -> MkGlobalIpToml app' _field _ipv4Sources _ipv6Sources)
          (f _app)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe GlobalIpField, b ~ Maybe GlobalIpField) =>
  LabelOptic "field" k GlobalIpToml GlobalIpToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkGlobalIpToml _app _field _ipv4Sources _ipv6Sources) ->
        fmap
          (\field' -> MkGlobalIpToml _app field' _ipv4Sources _ipv6Sources)
          (f _field)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ [UrlSource Ipv4], b ~ [UrlSource Ipv4]) =>
  LabelOptic "ipv4Sources" k GlobalIpToml GlobalIpToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkGlobalIpToml _app _field _ipv4Sources _ipv6Sources) ->
        fmap
          (\ipv4Sources' -> MkGlobalIpToml _app _field ipv4Sources' _ipv6Sources)
          (f _ipv4Sources)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ [UrlSource Ipv6], b ~ [UrlSource Ipv6]) =>
  LabelOptic "ipv6Sources" k GlobalIpToml GlobalIpToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkGlobalIpToml _app _field _ipv4Sources _ipv6Sources) ->
        fmap
          (MkGlobalIpToml _app _field _ipv4Sources)
          (f _ipv6Sources)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML GlobalIpToml where
  tomlDecoder =
    MkGlobalIpToml
      <$> getFieldOptWith (parseGlobalIpApp tomlDecoder) "app"
      <*> getFieldOptWith tomlDecoder "field"
      -- We need to use 'getFieldOptWith' so that these keys are optional,
      -- but we do not need Maybe in the type. That is, we can just use
      -- [UrlSource a] rather than Maybe [UrlSource a], as there is currently
      -- no need to distinguish Nothing vs. []. Thus we use getFieldOptWith
      -- and map Nothing to the empty list.
      --
      -- Previously we had getFieldOpt which caused a bug where the keys were
      -- mandatory.
      <*> decodeSourcesOptional "ipv4-src"
      <*> decodeSourcesOptional "ipv6-src"

-- | @since 0.1
fieldKey :: (IsString s) => s
fieldKey = "field"

-- | @since 0.1
ipv4SrcKey :: (IsString s) => s
ipv4SrcKey = "ipv4-src"

-- | @since 0.1
ipv6SrcKey :: (IsString s) => s
ipv6SrcKey = "ipv6-src"

decodeSourcesOptional :: Text -> Decoder [UrlSource a]
decodeSourcesOptional key = fromMaybe [] <$> getFieldOptWith decodeSources key

decodeSources :: Decoder [UrlSource a]
decodeSources = getArrayOf decodeSource

decodeSource :: Decoder (UrlSource a)
decodeSource = MkUrlSource <$> tomlDecoder
