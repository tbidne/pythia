{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for querying system information.
--
-- @since 0.1.0.0
module Main (main) where

import Control.Applicative qualified as A
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    ParserInfo (..),
    (<**>),
  )
import Options.Applicative qualified as OApp
import Options.Applicative.Help (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import Pythia qualified
import Pythia.Data (QueryResult)
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Services.Battery (BatteryApp (..))
import Pythia.Services.Network.Connection (NetConnApp (..))
import Pythia.Services.Network.IP.Global
  ( GlobalIpApp (..),
    GlobalIpCommand (..),
    IpStrategy (..),
    Ipv4Command,
    Ipv6Command,
  )
import Pythia.Services.Network.IP.Global qualified as Pythia
import Pythia.Services.Network.IP.Local (LocalIpApp (..))

-- | Runs the executable.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  cmd <- OApp.execParser parserInfo
  case cmd of
    Battery app -> case app of -- Pythia.queryBattery bsa >>= prettyPrint
      Nothing -> Pythia.queryBattery >>= prettyPrint
      Just bsa -> Pythia.queryBatteryApp bsa >>= prettyPrint
    NetConnection nca -> Pythia.queryConnection nca >>= prettyPrint
    NetIPLocal lia -> Pythia.queryLocalIP lia >>= prettyPrint
    NetIPGlobal gia is -> Pythia.queryGlobalIPStrategy is gia >>= prettyPrint
    NetIPGlobalCustom gic -> Pythia.queryGlobalCustom gic >>= prettyPrint

prettyPrint :: PrettyPrinter a => QueryResult a -> IO ()
prettyPrint = putStrLn . Pythia.prettyQueryResult

data PythiaCommand
  = Battery (Maybe BatteryApp)
  | NetConnection NetConnApp
  | NetIPLocal LocalIpApp
  | NetIPGlobal GlobalIpApp IpStrategy
  | NetIPGlobalCustom GlobalIpCommand
  deriving (Eq, Show)

parserInfo :: ParserInfo PythiaCommand
parserInfo =
  ParserInfo
    { infoParser = cmdParser,
      infoFullDesc = True,
      infoProgDesc = Chunk Nothing,
      infoHeader = Chunk Nothing,
      infoFooter = Chunk Nothing,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }

cmdParser :: Parser PythiaCommand
cmdParser =
  OApp.hsubparser
    ( mkCommand "battery" parseBattery batStateTxt
        <> mkCommand "net-conn" parseNetConn netConnTxt
        <> mkCommand "ip-local" parseIpLocal ipLocalTxt
        <> mkCommand "ip-global" parseIpGlobal ipGlobalTxt
    )
    <**> OApp.helper
    <**> version
  where
    batStateTxt =
      OApp.progDesc $
        "Queries the battery state (i.e. charging status and current percentage). "
          <> "If no app is supplied then we will try all in the following order: "
          <> "[sysfs, acpi, upower]."
    netConnTxt = OApp.progDesc "Queries the network connection status for a given device."
    ipLocalTxt = OApp.progDesc "Queries the local IP addresses associated to a given device."
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
parseBattery =
  Battery
    <$> OApp.option
      reader
      (OApp.value Nothing <> OApp.long "app" <> OApp.metavar "APP" <> OApp.help helpTxt)
  where
    helpTxt = "App must be one of [acpi | sysfs | upower | <custom command>]"
    reader = do
      a <- OApp.str
      case a of
        "acpi" -> pure $ Just BatteryAcpi
        "sysfs" -> pure $ Just BatterySysFs
        "upower" -> pure $ Just BatteryUPower
        custom -> pure $ Just $ BatteryCustom custom

parseNetConn :: Parser PythiaCommand
parseNetConn = do
  app <-
    OApp.option
      OApp.str
      (OApp.long "app" <> OApp.metavar "APP" <> OApp.help helpTxt)
  device <-
    OApp.option
      OApp.str
      (OApp.long "device" <> OApp.metavar "NAME" <> OApp.help deviceTxt)
  pure $ case app of
    "nmcli" -> NetConnection $ NetConNmCli device
    custom -> NetConnection $ NetConCustom device custom
  where
    helpTxt = "App must be one of [nmcli | <custom command>]"
    deviceTxt = "The name of the network device e.g. wlp0s20f3"

parseIpLocal :: Parser PythiaCommand
parseIpLocal = do
  app <-
    OApp.option
      OApp.str
      (OApp.long "app" <> OApp.metavar "APP" <> OApp.help helpTxt)
  device <-
    OApp.option
      OApp.str
      (OApp.long "device" <> OApp.metavar "NAME" <> OApp.help deviceTxt)

  pure $ case app of
    "ifconfig" -> NetIPLocal $ LocalIfConfig device
    "nmcli" -> NetIPLocal $ LocalNmCli device
    custom -> NetIPLocal $ LocalCustom device custom
  where
    helpTxt = "App must be one of [ifconfig | nmcli | <custom command>]"
    deviceTxt = "The name of the network device e.g. wlp0s20f3"

parseIpGlobal :: Parser PythiaCommand
parseIpGlobal =
  OApp.hsubparser
    ( OApp.command "dig" (OApp.info digParser digText)
        <> OApp.command "curl" (OApp.info curlParser curlText)
        <> OApp.command "custom" (OApp.info customGlobalIPParser customText)
    )
  where
    digText =
      OApp.progDesc $
        "Queries IP addresses via the dig utility, "
          <> "using built-in URLs. Additional commands (e.g. dig <url>) can be "
          <> "provided via the ipv4 and ipv6 arguments."
    curlText =
      OApp.progDesc $
        "Queries IP addresses via the curl utility, "
          <> "using built-in URLs. Additional commands (e.g. curl <url>) can be "
          <> "provided via the ipv4 and ipv6 arguments."
    customText =
      OApp.progDesc $
        "Queries IP addresses via custom command(s), "
          <> " with no built-in URLs being used. At least one of ipv4 or ipv6"
          <> " is mandatory."

digParser :: Parser PythiaCommand
digParser = do
  mIpv4 <- A.optional (ipv4Option "dig")
  mIpv6 <- A.optional ipv6Option
  pure $ ipsToPythia GlobalDig mIpv4 mIpv6

curlParser :: Parser PythiaCommand
curlParser = do
  mIpv4 <- A.optional (ipv4Option "curl")
  mIpv6 <- A.optional ipv6Option
  pure $ ipsToPythia GlobalCurl mIpv4 mIpv6

ipv4Option :: String -> Parser Ipv4Command
ipv4Option exCmd =
  OApp.option
    OApp.str
    ( OApp.long "ipv4"
        <> OApp.metavar "CMD"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "Custom command for retrieving the IPv4 address"
        <> " e.g. "
        <> exCmd
        <> " http://whatismyip.akamai.com/"

ipv6Option :: Parser Ipv6Command
ipv6Option =
  OApp.option
    OApp.str
    ( OApp.long "ipv6"
        <> OApp.metavar "CMD"
        <> OApp.help helpTxt
    )
  where
    helpTxt = "Custom command for retrieving the IPv6 address"

ipsToPythia :: GlobalIpApp -> Maybe Ipv4Command -> Maybe Ipv6Command -> PythiaCommand
ipsToPythia app mIpv4 mIpv6 = case (mIpv4, mIpv6) of
  (Nothing, Nothing) -> NetIPGlobal app Defaults
  (Just ipv4s, Nothing) -> NetIPGlobal app (CustomWithDefaults (GIpv4Command [ipv4s]))
  (Nothing, Just ipv6s) -> NetIPGlobal app (CustomWithDefaults (GIpv6Command [ipv6s]))
  (Just ipv4s, Just ipv6s) -> NetIPGlobal app (CustomWithDefaults (GIpBothCommand [ipv4s] [ipv6s]))

customGlobalIPParser :: Parser PythiaCommand
customGlobalIPParser =
  NetIPGlobalCustom
    . combineIps
    <$> parseIpv4AndIpv6
  where
    parseIpv4AndIpv6 =
      A.some $
        Left <$> ipv4Option "curl" <|> Right <$> ipv6Option
    combineIps xs =
      let (ipv4s, ipv6s) = foldl' splitIps ([], []) xs
       in GIpBothCommand ipv4s ipv6s
    splitIps (ip4s, ip6s) (Left x) = (x : ip4s, ip6s)
    splitIps (ip4s, ip6s) (Right y) = (ip4s, y : ip6s)

mkCommand :: String -> Parser a -> OApp.InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OApp.command cmdTxt (OApp.info parser helpTxt)
