{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides the arg parsing functionality.
--
-- @since 0.1.0.0
module Args
  ( PythiaCommand (..),
    NetInterfaceSelector (..),
    parserInfo,
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
import Pythia qualified
import Pythia.Data (RunApp (..))
import Pythia.Prelude
import Pythia.Services.Battery (BatteryApp (..), BatteryConfig (..))
import Pythia.Services.Network.GlobalIP.Types
  ( GlobalIpApp (..),
    GlobalIpConfig (..),
    GlobalIpRequest (..),
    GlobalIpSources (..),
    UrlSource (..),
  )
import Pythia.Services.Network.NetInterface (NetInterfaceApp (..), NetInterfaceConfig (..))
import Pythia.Services.Network.Types (Device (..), IpType (..))

-- | Possible commands
--
-- @since 0.1.0.0
data PythiaCommand
  = Battery BatteryConfig
  | NetInterface NetInterfaceConfig (Maybe NetInterfaceSelector)
  | NetIpGlobal GlobalIpConfig
  deriving stock (Eq, Show)

-- | Extra optiona for NetInterface.
--
-- @since 0.1.0.0
data NetInterfaceSelector
  = NetInterfaceSelectorName
  | NetInterfaceSelectorIpv4
  | NetInterfaceSelectorIpv6
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
        <> mkCommand "net-if" parseNetInterface netInterfaceTxt
        <> mkCommand "global-ip" parseIpGlobal ipGlobalTxt
    )
    <**> OApp.helper
    <**> version
  where
    batStateTxt =
      OApp.progDesc "Queries the battery state."
    netInterfaceTxt = OApp.progDesc "Queries network interfaces."
    ipGlobalTxt = OApp.progDesc "Queries the global IP addresses."

version :: Parser (a -> a)
version = OApp.infoOption txt (OApp.long "version" <> OApp.short 'v')
  where
    txt =
      Pythia.joinNewlines
        [ "Pythia",
          "Version: " <> $$(PV.packageVersionStringTH "pythia.cabal"),
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

parseBattery :: Parser PythiaCommand
parseBattery = do
  app <-
    OApp.option
      reader
      ( OApp.value Many
          <> OApp.long "app"
          <> OApp.short 'a'
          <> OApp.metavar "APP"
          <> OApp.help helpTxt
      )
  pure $ Battery $ MkBatteryConfig app
  where
    helpTxt = "App must be one of [acpi | sysfs | upower]."
    reader = do
      a <- OApp.str
      case a of
        "acpi" -> pure $ Single BatteryAcpi
        "sysfs" -> pure $ Single BatterySysFs
        "upower" -> pure $ Single BatteryUPower
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized battery app: " <> T.unpack a

parseNetInterface :: Parser PythiaCommand
parseNetInterface = do
  app <- netInterfaceAppOption
  device <- netInterfaceDeviceOption
  val <- parseNetInterfaceSelector
  pure $ NetInterface (MkNetInterfaceConfig app device) val

parseNetInterfaceSelector :: Parser (Maybe NetInterfaceSelector)
parseNetInterfaceSelector =
  A.optional $
    OApp.option
      readApp
      ( OApp.long "field"
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
        "name" -> pure $ NetInterfaceSelectorName
        "ipv4" -> pure $ NetInterfaceSelectorIpv4
        "ipv6" -> pure $ NetInterfaceSelectorIpv6
        _ -> OApp.readerAbort $ ErrorMsg $ "Unrecognized network interface field: " <> T.unpack a

netInterfaceAppOption :: Parser (RunApp NetInterfaceApp)
netInterfaceAppOption =
  OApp.option
    readApp
    ( OApp.value Many
        <> OApp.long "app"
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
        "nmcli" -> pure $ Single NetInterfaceNmCli
        "ip" -> pure $ Single NetInterfaceIp
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

parseIpGlobal :: Parser PythiaCommand
parseIpGlobal = do
  app <- ipAppOption
  ipType <- ipTypeOption
  ipv4Urls <- ipv4SrcOption
  ipv6Urls <- ipv6SrcOption
  pure $ NetIpGlobal $ MkGlobalIpConfig app ipType (MkGlobalIpSources ipv4Urls ipv6Urls)

ipAppOption :: Parser (RunApp GlobalIpApp)
ipAppOption =
  OApp.option
    readApp
    ( OApp.value Many
        <> OApp.long "app"
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
        "dig" -> pure $ Single GlobalIpDig
        "curl" -> pure $ Single GlobalIpCurl
        _ ->
          OApp.readerAbort $
            ErrorMsg $
              "Unrecognized network interface app: " <> T.unpack a

ipTypeOption :: Parser GlobalIpRequest
ipTypeOption =
  OApp.option
    readIpType
    ( OApp.value GlobalIpRequestIpv4
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
        "ipv4" -> pure GlobalIpRequestIpv4
        "ipv6" -> pure GlobalIpRequestIpv6
        "both" -> pure GlobalIpRequestBoth
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
      "Custom server URL for retrieving the IPv4 address e.g. "
        <> "http://whatismyip.akamai.com/. Can be specified multiple times. "
        <> "Overrides the defaults."

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
      "Custom server URL for retrieving the IPv6 address. Can be specified "
        <> "multiple times. Overrides the defaults."

mkCommand :: String -> Parser a -> OApp.InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OApp.command cmdTxt (OApp.info parser helpTxt)
