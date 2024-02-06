{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Pythia.Runner.Toml
  ( Toml (..),
    ConfigException (..),
    combineConfigs,
  )
where

import Pythia
  ( BatteryApp,
    GlobalIpApp,
    IpType (Ipv4, Ipv6),
    MemoryApp,
    NetInterfaceApp,
    UrlSource,
  )
import Pythia.Prelude
import Pythia.Runner.Command
  ( PythiaCommand1,
    PythiaCommand2,
    PythiaCommandP
      ( BatteryCmd,
        GlobalIpCmd,
        MemoryCmd,
        NetConnCmd,
        NetInterfaceCmd,
        TimeCmd
      ),
  )
import Pythia.Runner.Command.Battery
  ( BatteryField (BatteryFieldDefault),
    BatteryToml,
  )
import Pythia.Runner.Command.GlobalIp
  ( GlobalIpField
      ( GlobalIpFieldBoth,
        GlobalIpFieldIpv4,
        GlobalIpFieldIpv6
      ),
    GlobalIpToml,
  )
import Pythia.Runner.Command.Memory
  ( MemoryField (MemoryFieldDefault),
    MemoryToml,
    MemoryUnits (MemoryUnitsBytes),
  )
import Pythia.Runner.Command.NetConn
  ( NetConnField (NetConnFieldDefault),
    NetConnToml,
  )
import Pythia.Runner.Command.NetInterface
  ( NetInterfaceDevice (NetInterfaceDeviceNone),
    NetInterfaceField (NetInterfaceFieldDefault),
    NetInterfaceToml,
  )
import Pythia.Runner.Command.Time
  ( TimeFormat (TimeFormatDefault),
    TimeToml,
    TimezoneDest (TimezoneDestLocal),
  )
import Pythia.Runner.These (These (That, These, This))
import TOML (DecodeTOML, getFieldOptWith)
import TOML.Decode (DecodeTOML (tomlDecoder))

-- | @since 0.1
data Toml = MkToml
  { -- | @since 0.1
    batteryToml :: Maybe BatteryToml,
    -- | @since 0.1
    globalIpToml :: Maybe GlobalIpToml,
    -- | @since 0.1
    memoryToml :: Maybe MemoryToml,
    -- | @since 0.1
    netInterfaceToml :: Maybe NetInterfaceToml,
    -- | @since 0.1
    netConnToml :: Maybe NetConnToml,
    -- | @since 0.1
    timeToml :: Maybe TimeToml
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe BatteryToml, b ~ Maybe BatteryToml) =>
  LabelOptic "batteryToml" k Toml Toml a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkToml
             _batteryApp
             _globalIpToml
             _memoryToml
             _netInterfaceToml
             _netConnToml
             _timeToml
           ) ->
          fmap
            ( \batteryApp' ->
                MkToml
                  batteryApp'
                  _globalIpToml
                  _memoryToml
                  _netInterfaceToml
                  _netConnToml
                  _timeToml
            )
            (f _batteryApp)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe GlobalIpToml, b ~ Maybe GlobalIpToml) =>
  LabelOptic "globalIpToml" k Toml Toml a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkToml
             _batteryApp
             _globalIpToml
             _memoryToml
             _netInterfaceToml
             _netConnToml
             _timeToml
           ) ->
          fmap
            ( \globalIpToml' ->
                MkToml
                  _batteryApp
                  globalIpToml'
                  _memoryToml
                  _netInterfaceToml
                  _netConnToml
                  _timeToml
            )
            (f _globalIpToml)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe MemoryToml, b ~ Maybe MemoryToml) =>
  LabelOptic "memoryToml" k Toml Toml a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkToml
             _batteryApp
             _globalIpToml
             _memoryToml
             _netInterfaceToml
             _netConnToml
             _timeToml
           ) ->
          fmap
            ( \memoryToml' ->
                MkToml
                  _batteryApp
                  _globalIpToml
                  memoryToml'
                  _netInterfaceToml
                  _netConnToml
                  _timeToml
            )
            (f _memoryToml)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NetInterfaceToml, b ~ Maybe NetInterfaceToml) =>
  LabelOptic "netInterfaceToml" k Toml Toml a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkToml
             _batteryApp
             _globalIpToml
             _memoryToml
             _netInterfaceToml
             _netConnToml
             _timeToml
           ) ->
          fmap
            ( \netInterfaceToml' ->
                MkToml
                  _batteryApp
                  _globalIpToml
                  _memoryToml
                  netInterfaceToml'
                  _netConnToml
                  _timeToml
            )
            (f _netInterfaceToml)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NetConnToml, b ~ Maybe NetConnToml) =>
  LabelOptic "netConnToml" k Toml Toml a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkToml
             _batteryApp
             _globalIpToml
             _memoryToml
             _netInterfaceToml
             _netConnToml
             _timeToml
           ) ->
          fmap
            ( \netConnToml' ->
                MkToml
                  _batteryApp
                  _globalIpToml
                  _memoryToml
                  _netInterfaceToml
                  netConnToml'
                  _timeToml
            )
            (f _netConnToml)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe TimeToml, b ~ Maybe TimeToml) =>
  LabelOptic "timeToml" k Toml Toml a b
  where
  labelOptic =
    lensVL
      $ \f
         ( MkToml
             _batteryApp
             _globalIpToml
             _memoryToml
             _netInterfaceToml
             _netConnToml
             _timeToml
           ) ->
          fmap
            ( MkToml
                _batteryApp
                _globalIpToml
                _memoryToml
                _netInterfaceToml
                _netConnToml
            )
            (f _timeToml)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML Toml where
  tomlDecoder =
    MkToml
      <$> getFieldOptWith tomlDecoder "battery"
      <*> getFieldOptWith tomlDecoder "global-ip"
      <*> getFieldOptWith tomlDecoder "memory"
      <*> getFieldOptWith tomlDecoder "net-if"
      <*> getFieldOptWith tomlDecoder "net-conn"
      <*> getFieldOptWith tomlDecoder "time"

-- | @since 0.1
newtype ConfigException
  = -- | @since 0.1
    MissingApp String
  deriving stock (Eq, Show)

-- | @since 0.1
instance Exception ConfigException where
  displayException (MissingApp t) = "Missing app: " <> t

-- | Combines the @PythiaCommand Phase1@ (CLI args) with the Toml configuration
-- to produce the final @PythiaCommand Phase2@, which can then be handed off to
-- Pythia.
--
-- Generally this means filling in defaults for missing fields, though a
-- missing app will cause a failure.
--
-- | @since 0.1
combineConfigs ::
  PythiaCommand1 ->
  Maybe Toml ->
  Either ConfigException PythiaCommand2
combineConfigs (BatteryCmd mapp mfield) =
  combineBatteryConfig mapp mfield
    . preview (_Just % #batteryToml % _Just)
combineConfigs (GlobalIpCmd mapp fields) =
  combineGlobalIpConfig mapp fields
    . preview (_Just % #globalIpToml % _Just)
combineConfigs (MemoryCmd mapp mfield format) =
  combineMemoryConfig mapp mfield format
    . preview (_Just % #memoryToml % _Just)
combineConfigs (NetInterfaceCmd mapp mdevice mfield) =
  combineNetInterfaceConfig mapp mdevice mfield
    . preview (_Just % #netInterfaceToml % _Just)
combineConfigs (NetConnCmd mapp mfield) =
  combineNetConnConfig mapp mfield
    . preview (_Just % #netConnToml % _Just)
combineConfigs (TimeCmd mdest mformat) =
  combineTimeConfig mdest mformat
    . preview (_Just % #timeToml % _Just)

combineBatteryConfig ::
  Maybe BatteryApp ->
  Maybe BatteryField ->
  Maybe BatteryToml ->
  Either ConfigException PythiaCommand2
combineBatteryConfig cliMApp cliMField toml = case mapp of
  Nothing -> Left $ MissingApp "battery"
  Just app -> Right $ BatteryCmd app field
  where
    mapp = mAltLens cliMApp toml #app
    field =
      fromMaybe
        BatteryFieldDefault
        (mAltLens cliMField toml #field)

combineGlobalIpConfig ::
  Maybe GlobalIpApp ->
  ( Maybe GlobalIpField,
    [UrlSource Ipv4],
    [UrlSource Ipv6]
  ) ->
  Maybe GlobalIpToml ->
  Either ConfigException PythiaCommand2
combineGlobalIpConfig cliMApp (mfield, ipv4Srcs, ipv6Srcs) toml = case mapp of
  Nothing -> Left $ MissingApp "global-ip"
  Just app -> Right $ GlobalIpCmd app sources
  where
    mapp = mAltLens cliMApp toml #app
    sources =
      case mAltLens mfield toml #field of
        Nothing -> This ipv4Srcs'
        Just GlobalIpFieldIpv4 -> This ipv4Srcs'
        Just GlobalIpFieldIpv6 -> That ipv6Srcs'
        Just GlobalIpFieldBoth -> These ipv4Srcs' ipv6Srcs'

    ipv4Srcs' = chooseSrcs ipv4Srcs #ipv4Sources
    ipv6Srcs' = chooseSrcs ipv6Srcs #ipv6Sources

    -- If CLI is non-empty, take those. Otherwise take (potentially empty)
    -- TOML.
    chooseSrcs :: [UrlSource a] -> Lens' GlobalIpToml [UrlSource a] -> [UrlSource a]
    chooseSrcs xs l = case xs of
      [] -> fromMaybe [] (toml ^? (_Just % l))
      ys@(_ : _) -> ys

combineMemoryConfig ::
  Maybe MemoryApp ->
  Maybe MemoryField ->
  Maybe MemoryUnits ->
  Maybe MemoryToml ->
  Either ConfigException PythiaCommand2
combineMemoryConfig cliMApp cliMField cliMFormat toml =
  case mapp of
    Nothing -> Left $ MissingApp "memory"
    Just app -> Right $ MemoryCmd app field units
  where
    mapp = mAltLens cliMApp toml #app

    field = fromMaybe MemoryFieldDefault (mAltLens cliMField toml #field)
    units = fromMaybe MemoryUnitsBytes (mAltLens cliMFormat toml #units)

combineNetInterfaceConfig ::
  Maybe NetInterfaceApp ->
  Maybe NetInterfaceDevice ->
  Maybe NetInterfaceField ->
  Maybe NetInterfaceToml ->
  Either ConfigException PythiaCommand2
combineNetInterfaceConfig cliMApp cliMDevice cliMField toml =
  case mapp of
    Nothing -> Left $ MissingApp "net-if"
    Just app -> Right $ NetInterfaceCmd app mdevice field
  where
    mapp = mAltLens cliMApp toml #app

    mdevice =
      fromMaybe
        NetInterfaceDeviceNone
        (mAltLens cliMDevice toml #device)

    field = fromMaybe NetInterfaceFieldDefault (mAltLens cliMField toml #field)

combineNetConnConfig ::
  Maybe NetInterfaceApp ->
  Maybe NetConnField ->
  Maybe NetConnToml ->
  Either ConfigException PythiaCommand2
combineNetConnConfig cliMApp cliMField toml = case mapp of
  Nothing -> Left $ MissingApp "net-conn"
  Just app -> Right $ NetConnCmd app field
  where
    mapp = mAltLens cliMApp toml #app
    field = fromMaybe NetConnFieldDefault (mAltLens cliMField toml #field)

combineTimeConfig ::
  Maybe TimezoneDest ->
  Maybe TimeFormat ->
  Maybe TimeToml ->
  Either ConfigException PythiaCommand2
combineTimeConfig cliMDest cliMFormat toml = Right $ TimeCmd dest format
  where
    dest = fromMaybe TimezoneDestLocal $ mAltLens cliMDest toml #dest

    format =
      fromMaybe
        TimeFormatDefault
        (mAltLens cliMFormat toml #format)

mAltLens :: Maybe a -> Maybe s -> Lens' s (Maybe a) -> Maybe a
mAltLens x f l = x <|> f ^? (_Just % l % _Just)
