{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides the arg parsing functionality.
--
-- @since 0.1
module Pythia.Args
  ( -- * Primary Function
    parserInfo,

    -- * Types

    -- ** Main
    PythiaCommand (..),
    BatteryField (..),
    MemoryField (..),
    MemoryFormat (..),
    NetInterfaceField (..),
    NetConnField (..),
    TimezoneDest (..),

    -- ** Misc
    These (..),
  )
where

import Control.Applicative qualified as A
import Data.Text qualified as T
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative
  ( CommandFields,
    Mod,
    ParseError (..),
    Parser,
    ParserInfo (..),
    (<**>),
  )
import Options.Applicative qualified as OApp
import Options.Applicative.Help (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import Pythia.Prelude
import Pythia.Services.Battery.Types (BatteryApp (..))
import Pythia.Services.GlobalIp.Types
  ( GlobalIpApp (..),
    GlobalIpConfig (..),
    UrlSource (..),
  )
import Pythia.Services.Memory.Types (MemoryApp (..))
import Pythia.Services.NetInterface.Types (NetInterfaceApp (..))
import Pythia.Services.Types.Network (Device (..), IpType (..))
import Pythia.Utils (Pretty (..), (<+>))
import Pythia.Utils qualified as U

-- | So we don't have to add @these@.
--
-- @since 0.1
type These :: Type -> Type -> Type
data These a b
  = This a
  | That b
  | These a b
  deriving stock (Eq, Show)

-- | Determines how to print memory.
--
-- @since 0.1
type MemoryFormat :: Type
data MemoryFormat
  = MemoryBytes
  | MemoryPercentage
  deriving stock (Eq, Show)

-- | Possible commands
--
-- @since 0.1
type PythiaCommand :: Type
data PythiaCommand
  = BatteryCmd BatteryApp BatteryField
  | MemoryCmd MemoryApp MemoryField MemoryFormat
  | NetInterfaceCmd NetInterfaceApp (Maybe Device) NetInterfaceField
  | NetConnCmd NetInterfaceApp NetConnField
  | NetIpGlobalCmd (GlobalIpConfig (These [UrlSource 'Ipv4] [UrlSource 'Ipv6]))
  | TimeCmd TimezoneDest (Maybe String)
  deriving stock (Eq, Show)

-- | Extra option for BatteryCmd.
--
-- @since 0.1
type BatteryField :: Type
data BatteryField
  = BatteryFieldDefault
  | BatteryFieldPercentage
  | BatteryFieldStatus
  deriving stock (Eq, Show)

-- | Extra option for MemoryCmd.
--
-- @since 0.1
type MemoryField :: Type
data MemoryField
  = MemoryFieldDefault
  | MemoryFieldTotal
  | MemoryFieldUsed
  | MemoryFieldFree
  deriving stock (Eq, Show)

-- | Extra option for NetInterfaceCmd.
--
-- @since 0.1
type NetInterfaceField :: Type
data NetInterfaceField
  = NetInterfaceFieldDefault
  | NetInterfaceFieldName
  | NetInterfaceFieldIpv4
  | NetInterfaceFieldIpv6
  deriving stock (Eq, Show)

-- | Extra option for NetConnCmd.
--
-- @since 0.1
type NetConnField :: Type
data NetConnField
  = NetConnFieldDefault
  | NetConnFieldDevice
  | NetConnFieldType
  | NetConnFieldName
  | NetConnFieldIpv4
  | NetConnFieldIpv6
  deriving stock (Eq, Show)

-- | Extra option for GlobalIpCmd.
--
-- @since 0.1
type GlobalIpField :: Type
data GlobalIpField
  = GlobalIpFieldIpv4
  | GlobalIpFieldIpv6
  | GlobalIpFieldBoth
  deriving stock (Eq, Show)

-- | Time type.
--
-- @since 0.1
type TimezoneDest :: Type
data TimezoneDest
  = TimezoneDestLocal
  | TimezoneDestUTC
  | TimezoneDestTZ Text
  deriving stock (Eq, Show)

-- | Optparse-Applicative info.
parserInfo :: ParserInfo PythiaCommand
parserInfo =
  ParserInfo
    { infoParser = cmdParser,
      infoFullDesc = True,
      infoProgDesc = Chunk desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "Pythia: A tool for querying system information."
    footer = Just $ fromString ""
    desc =
      Just $
        "\nPythia queries system information. This is achieved by "
          <> "using applications on the machine whose output pythia knows how to "
          <> "parse. For instance, pythia can retrieve battery information by "
          <> "using acpi, upower, or reading /sys/class directly. In general, "
          <> "if the application is unspecified then pythia will try all "
          <> "applications that exist on the current system, returning the first "
          <> "success."

cmdParser :: Parser PythiaCommand
cmdParser =
  OApp.hsubparser
    ( mkCommand "battery" parseBattery batStateTxt
        <> mkCommand "global-ip" parseIpGlobal ipGlobalTxt
        <> mkCommand "memory" parseMemory memoryTxt
        <> mkCommand "net-if" parseNetInterface netInterfaceTxt
        <> mkCommand "net-conn" parseNetConn netConnTxt
        <> mkCommand "time" parseTime timeTxt
    )
    <**> OApp.helper
    <**> version
  where
    batStateTxt =
      OApp.progDesc "Queries the battery state."
    memoryTxt = OApp.progDesc "Queries memory usage."
    netInterfaceTxt = OApp.progDesc "Queries network interfaces."
    netConnTxt = OApp.progDesc "Queries network interfaces for a live connection."
    ipGlobalTxt = OApp.progDesc "Queries the global IP addresses."
    timeTxt = OApp.progDesc "Queries the system time."

version :: Parser (a -> a)
version = OApp.infoOption txt (OApp.long "version" <> OApp.short 'v')
  where
    txt =
      T.unpack $
        toText $
          U.vsep
            [ pretty @Text "Pythia",
              pretty @Text "Version:" <+> pretty $$(PV.packageVersionTextTH "pythia.cabal"),
              pretty @Text "Revision:" <+> pretty @Text $(GitRev.gitHash),
              pretty @Text "Date:" <+> pretty @Text $(GitRev.gitCommitDate)
            ]
    toText = U.renderStrict . U.layoutCompact

parseBattery :: Parser PythiaCommand
parseBattery = do
  app <-
    OApp.option
      reader
      ( OApp.long "app"
          <> OApp.short 'a'
          <> OApp.metavar "APP"
          <> OApp.help helpTxt
      )
  field <- parseBatteryField
  pure $ BatteryCmd app field
  where
    helpTxt = "App must be one of [acpi | sysfs | upower]."
    reader = do
      a <- OApp.str
      case a of
        "acpi" -> pure BatteryAppAcpi
        "sysfs" -> pure BatteryAppSysFs
        "upower" -> pure BatteryAppUPower
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized battery app: " <> T.unpack a

parseBatteryField :: Parser BatteryField
parseBatteryField =
  OApp.option
    readApp
    ( OApp.value BatteryFieldDefault
        <> OApp.long "field"
        <> OApp.short 'f'
        <> OApp.metavar "FIELD"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "If specified, prints only the given field. Must be one of [percentage | status]."
    readApp = do
      a <- OApp.str
      case a of
        "percentage" -> pure BatteryFieldPercentage
        "status" -> pure BatteryFieldStatus
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized battery field: " <> T.unpack a

parseMemory :: Parser PythiaCommand
parseMemory = do
  app <- parseMemoryAppOption
  field <- parseMemoryField
  percentage <- parseMemoryFormat
  pure $ MemoryCmd app field percentage

parseMemoryField :: Parser MemoryField
parseMemoryField =
  OApp.option
    readApp
    ( OApp.value MemoryFieldDefault
        <> OApp.long "field"
        <> OApp.short 'f'
        <> OApp.metavar "FIELD"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "If specified, prints only the given field. Must be one of [total | "
        <> "used | free]."
    readApp = do
      a <- OApp.str
      case a of
        "total" -> pure MemoryFieldTotal
        "used" -> pure MemoryFieldUsed
        "free" -> pure MemoryFieldFree
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized memory field: " <> T.unpack a

parseMemoryAppOption :: Parser MemoryApp
parseMemoryAppOption =
  OApp.option
    readApp
    ( OApp.long "app"
        <> OApp.short 'a'
        <> OApp.metavar "APP"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "App must be one of [free]."
    readApp = do
      a <- OApp.str
      case a of
        "free" -> pure MemoryAppFree
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized memory app: " <> T.unpack a

parseMemoryFormat :: Parser MemoryFormat
parseMemoryFormat =
  OApp.flag
    MemoryBytes
    MemoryPercentage
    ( OApp.long "percentage"
        <> OApp.short 'p'
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "If specified, values are written as a percentage rather than bytes."

parseNetInterface :: Parser PythiaCommand
parseNetInterface = do
  app <- netInterfaceAppOption
  device <- netInterfaceDeviceOption
  val <- parseNetInterfaceField
  pure $ NetInterfaceCmd app device val

parseNetInterfaceField :: Parser NetInterfaceField
parseNetInterfaceField =
  OApp.option
    readApp
    ( OApp.value NetInterfaceFieldDefault
        <> OApp.long "field"
        <> OApp.short 'f'
        <> OApp.metavar "FIELD"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "If specified, prints only the given field. Must be one of [name | ipv4 | ipv6]."
    readApp = do
      a <- OApp.str
      case a of
        "name" -> pure NetInterfaceFieldName
        "ipv4" -> pure NetInterfaceFieldIpv4
        "ipv6" -> pure NetInterfaceFieldIpv6
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized network interface field: " <> T.unpack a

netInterfaceAppOption :: Parser NetInterfaceApp
netInterfaceAppOption =
  OApp.option
    readApp
    ( OApp.long "app"
        <> OApp.short 'a'
        <> OApp.metavar "APP"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "App must be one of [nmcli | ip]."
    readApp = do
      a <- OApp.str
      case a of
        "nmcli" -> pure NetInterfaceAppNmCli
        "ip" -> pure NetInterfaceAppIp
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized network interface app: " <> T.unpack a

netInterfaceDeviceOption :: Parser (Maybe Device)
netInterfaceDeviceOption =
  OApp.option
    (Just . MkDevice <$> OApp.str)
    ( OApp.value Nothing
        <> OApp.long "device"
        <> OApp.short 'd'
        <> OApp.metavar "NAME"
        <> OApp.help deviceTxt
    )
  where
    deviceTxt = "The name of the network device to filter on e.g. wlp0s20f3"

parseNetConn :: Parser PythiaCommand
parseNetConn = NetConnCmd <$> parseApp <*> parseNetConnField
  where
    parseApp = netInterfaceAppOption

parseNetConnField :: Parser NetConnField
parseNetConnField =
  OApp.option
    readApp
    ( OApp.value NetConnFieldDefault
        <> OApp.long "field"
        <> OApp.short 'f'
        <> OApp.metavar "FIELD"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "If specified, prints only the given field. Must be one of"
        <> " [device | type | name | ipv4 | ipv6]."
    readApp = do
      a <- OApp.str
      case a of
        "device" -> pure NetConnFieldDevice
        "type" -> pure NetConnFieldType
        "name" -> pure NetConnFieldName
        "ipv4" -> pure NetConnFieldIpv4
        "ipv6" -> pure NetConnFieldIpv6
        _ ->
          OApp.readerAbort $
            ErrorMsg $
              "Unrecognized network interface field: "
                <> T.unpack a

parseIpGlobal :: Parser PythiaCommand
parseIpGlobal = do
  app <- ipAppOption
  ipType <- ipTypeOption
  ipv4Urls <- ipv4SrcOption
  ipv6Urls <- ipv6SrcOption

  pure $
    NetIpGlobalCmd $ case ipType of
      GlobalIpFieldIpv4 -> MkGlobalIpConfig app (This ipv4Urls)
      GlobalIpFieldIpv6 -> MkGlobalIpConfig app (That ipv6Urls)
      GlobalIpFieldBoth -> MkGlobalIpConfig app (These ipv4Urls ipv6Urls)

ipAppOption :: Parser GlobalIpApp
ipAppOption =
  OApp.option
    readApp
    ( OApp.long "app"
        <> OApp.short 'a'
        <> OApp.metavar "APP"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "App must be one of [dig | curl]."
    readApp = do
      a <- OApp.str
      case a of
        "dig" -> pure GlobalIpAppDig
        "curl" -> pure GlobalIpAppCurl
        _ ->
          OApp.readerAbort $
            ErrorMsg $
              "Unrecognized network interface app: " <> T.unpack a

ipTypeOption :: Parser GlobalIpField
ipTypeOption =
  OApp.option
    readIpType
    ( OApp.value GlobalIpFieldIpv4
        <> OApp.long "ip-type"
        <> OApp.short 't'
        <> OApp.metavar "TYPE"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "Whether to retrieve IPv4 or IPv6 address. One of: [ipv4 | ipv6 | "
        <> "both]. Defaults to ipv4."
    readIpType = do
      a <- OApp.str
      case a of
        "ipv4" -> pure GlobalIpFieldIpv4
        "ipv6" -> pure GlobalIpFieldIpv6
        "both" -> pure GlobalIpFieldBoth
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized ip type: " <> T.unpack a

ipv4SrcOption :: Parser [UrlSource 'Ipv4]
ipv4SrcOption =
  A.many $
    OApp.option
      OApp.str
      ( OApp.long "ipv4-src"
          <> OApp.metavar "URL"
          <> OApp.help helpTxt
      )
  where
    helpTxt =
      "Custom server URL for retrieving the IPv4 address e.g."
        <> " http://whatismyip.akamai.com/. Can be specified multiple times"
        <> " and overrides the defaults. These sources are only used if we"
        <> " query for IPv4 per --ip-type."

ipv6SrcOption :: Parser [UrlSource 'Ipv6]
ipv6SrcOption =
  A.many $
    OApp.option
      OApp.str
      ( OApp.long "ipv6-src"
          <> OApp.metavar "URL"
          <> OApp.help helpTxt
      )
  where
    helpTxt =
      "Custom server URL for retrieving the IPv6 address. Can be specified"
        <> " multiple times and overrides the defaults. These sources are"
        <> " only used if we query for IPv6 per --ip-type."

parseTime :: Parser PythiaCommand
parseTime = TimeCmd <$> parseTimezoneDest <*> parseTimeFormat

parseTimezoneDest :: Parser TimezoneDest
parseTimezoneDest =
  OApp.option
    readApp
    ( OApp.value TimezoneDestLocal
        <> OApp.long "dest"
        <> OApp.short 'd'
        <> OApp.metavar "[utc | TZ]"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Determines what timezone we return. If none is given we assume",
          " local time. If given, must be one of [utc | TZ] where TZ is a tz",
          " database label e.g. America/New_York. See",
          " https://en.wikipedia.org/wiki/Tz_database."
        ]
    readApp = do
      a <- OApp.str
      case a of
        "utc" -> pure TimezoneDestUTC
        other -> pure $ TimezoneDestTZ other

parseTimeFormat :: Parser (Maybe String)
parseTimeFormat =
  A.optional $
    OApp.option
      OApp.str
      ( OApp.long "format"
          <> OApp.short 'f'
          <> OApp.metavar "STR"
          <> OApp.help helpTxt
      )
  where
    helpTxt =
      "Glibc-style format string e.g. %Y-%m-%d for yyyy-mm-dd. Defaults to RFC822."
        <> " See https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime"

mkCommand :: String -> Parser a -> OApp.InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OApp.command cmdTxt (OApp.info parser helpTxt)
