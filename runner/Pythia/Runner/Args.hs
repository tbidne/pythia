{-# LANGUAGE UndecidableInstances #-}

-- | This modules provides the arg parsing functionality.
--
-- @since 0.1
module Pythia.Runner.Args
  ( -- * Primary Function
    Args (..),
    parserInfo,
  )
where

import Data.List qualified as L
import Data.Version (Version (versionBranch))
import Effects.Optparse (validOsPath)
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    ParserInfo (ParserInfo),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_pythia qualified as Paths
import Pythia.Prelude
import Pythia.Runner.Command
  ( PythiaCommand1,
    PythiaCommandP
      ( BatteryCmd,
        GlobalIpCmd,
        MemoryCmd,
        NetConnCmd,
        NetInterfaceCmd,
        TimeCmd
      ),
  )
import Pythia.Runner.Command.Battery (BatteryField)
import Pythia.Runner.Command.Battery qualified as Battery
import Pythia.Runner.Command.GlobalIp (GlobalIpField)
import Pythia.Runner.Command.GlobalIp qualified as GlobalIp
import Pythia.Runner.Command.Memory (MemoryField, MemoryUnits)
import Pythia.Runner.Command.Memory qualified as Memory
import Pythia.Runner.Command.NetConn (NetConnField)
import Pythia.Runner.Command.NetConn qualified as NetConn
import Pythia.Runner.Command.NetInterface
  ( NetInterfaceDevice,
    NetInterfaceField,
  )
import Pythia.Runner.Command.NetInterface qualified as NetInterface
import Pythia.Runner.Command.Time (TimeFormat, TimezoneDest)
import Pythia.Runner.Command.Time qualified as Time
import Pythia.Services.GlobalIp.Types (GlobalIpApp, UrlSource)
import Pythia.Services.Memory.Types (MemoryApp)
import Pythia.Services.NetInterface.Types (NetInterfaceApp)
import Pythia.Services.Types.Network (IpType (Ipv4, Ipv6))

-- | @since 0.1
data Args = MkArgs
  { -- | @since 0.1
    config :: Maybe OsPath,
    -- | @since 0.1
    noConfig :: Bool,
    -- | @since 0.1
    command :: PythiaCommand1
  }
  deriving stock (Eq, Show)

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe OsPath, b ~ Maybe OsPath) =>
  LabelOptic "config" k Args Args a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkArgs _config _noConfig _command) ->
          fmap (\config' -> MkArgs config' _noConfig _command) (f _config)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "noConfig" k Args Args a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkArgs _config _noConfig _command) ->
          fmap (\noConfig' -> MkArgs _config noConfig' _command) (f _noConfig)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ PythiaCommand1, b ~ PythiaCommand1) =>
  LabelOptic "command" k Args Args a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkArgs _config _noConfig _command) ->
          fmap (MkArgs _config _noConfig) (f _command)
  {-# INLINE labelOptic #-}

-- | Optparse-Applicative info.
parserInfo :: ParserInfo Args
parserInfo =
  ParserInfo
    { infoParser = parseArgs,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "Pythia: A tool for querying system information."
    footer = Just $ fromString ""
    desc =
      Chunk.paragraph
        $ "Pythia queries system information. This is achieved by "
        <> "using applications on the machine whose output pythia knows how to "
        <> "parse. For instance, pythia can retrieve battery information by "
        <> "using acpi, upower, or reading /sys/class directly. In general, "
        <> "if the application is unspecified then pythia will try all "
        <> "applications that exist on the current system, returning the first "
        <> "success."

parseArgs :: Parser Args
parseArgs =
  MkArgs
    <$> configParser
    <*> noConfigParser
    <*> cmdParser

configParser :: Parser (Maybe OsPath)
configParser =
  OA.optional
    $ OA.option
      validOsPath
      ( mconcat
          [ OA.short 'c',
            OA.long "config",
            OA.metavar "PATH",
            mkHelp "Path to toml config."
          ]
      )

noConfigParser :: Parser Bool
noConfigParser =
  OA.flag
    False
    True
    ( mconcat
        [ OA.long "no-config",
          OA.hidden,
          mkHelp helpTxt
        ]
    )
  where
    helpTxt =
      mconcat
        [ "Overrides toml file config regardless of how it was obtained i.e. ",
          "explicit --config or implicit reading of the XDG config file. ",
          "Used for when a config file exists at the expected XDG ",
          "location, but we want to ignore it."
        ]

cmdParser :: Parser PythiaCommand1
cmdParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "battery" parseBattery batStateTxt,
          mkCommand "global-ip" parseGlobalIp ipGlobalTxt,
          mkCommand "memory" parseMemory memoryTxt,
          mkCommand "net-conn" parseNetConn netConnTxt,
          mkCommand "net-if" parseNetInterface netInterfaceTxt,
          mkCommand "time" parseTime timeTxt
        ]
    )
    <**> OA.helper
    <**> version
  where
    batStateTxt = mkCmdDesc "Queries the battery state."
    memoryTxt = mkCmdDesc "Queries memory usage."
    netInterfaceTxt = mkCmdDesc "Queries network interfaces."
    netConnTxt = mkCmdDesc "Queries network interfaces for a live connection."
    ipGlobalTxt = mkCmdDesc "Queries the global IP addresses."
    timeTxt = mkCmdDesc "Queries the system time."

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')
  where
    versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

parseBattery :: Parser PythiaCommand1
parseBattery = do
  app <-
    OA.optional
      $ OA.option
        (Battery.parseBatteryApp OA.str)
        ( OA.long "app"
            <> OA.short 'a'
            <> OA.metavar "(acpi | sysfs | upower)"
            <> mkHelp helpTxt
        )
  field <- parseBatteryField
  pure $ BatteryCmd app field
  where
    helpTxt = "Specifies the app to use."

parseBatteryField :: Parser (Maybe BatteryField)
parseBatteryField =
  OA.optional
    $ OA.option
      (Battery.parseBatteryField OA.str)
      ( OA.long Battery.fieldKey
          <> OA.short 'f'
          <> OA.metavar "(default | percentage | status)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "If specified, prints only the given field."

parseGlobalIp :: Parser PythiaCommand1
parseGlobalIp = do
  app <- parseGlobalIpApp
  ipType <- parseGlobalIpField
  ipv4Urls <- parseIpv4Sources
  ipv6Urls <- parseIpv6Sources

  pure $ GlobalIpCmd app (ipType, ipv4Urls, ipv6Urls)

parseGlobalIpApp :: Parser (Maybe GlobalIpApp)
parseGlobalIpApp =
  OA.optional
    $ OA.option
      (GlobalIp.parseGlobalIpApp OA.str)
      ( OA.long "app"
          <> OA.short 'a'
          <> OA.metavar "(curl | dig)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "Specifies the app to use."

parseGlobalIpField :: Parser (Maybe GlobalIpField)
parseGlobalIpField =
  OA.optional
    $ OA.option
      (GlobalIp.parseGlobalIpField OA.str)
      ( OA.long GlobalIp.fieldKey
          <> OA.short 'f'
          <> OA.metavar "(ipv4 | ipv6 | both)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "Whether to retrieve IPv4 or IPv6 address. Defaults to ipv4."

parseIpv4Sources :: Parser [UrlSource Ipv4]
parseIpv4Sources =
  OA.many
    $ OA.option
      OA.str
      ( OA.long GlobalIp.ipv4SrcKey
          <> OA.metavar "URL"
          <> mkHelp helpTxt
      )
  where
    helpTxt =
      "Custom server URL for retrieving the IPv4 address e.g."
        <> " http://whatismyip.akamai.com/. Can be specified multiple times"
        <> " and overrides the defaults. These sources are only used if we"
        <> " query for IPv4 per --field."

parseIpv6Sources :: Parser [UrlSource Ipv6]
parseIpv6Sources =
  OA.many
    $ OA.option
      OA.str
      ( OA.long GlobalIp.ipv6SrcKey
          <> OA.metavar "URL"
          <> mkHelp helpTxt
      )
  where
    helpTxt =
      "Custom server URL for retrieving the IPv6 address. Can be specified"
        <> " multiple times and overrides the defaults. These sources are"
        <> " only used if we query for IPv6 per --field."

parseMemory :: Parser PythiaCommand1
parseMemory = do
  app <- parseMemoryApp
  field <- parseMemoryField
  percentage <- parseMemoryUnits
  pure $ MemoryCmd app field percentage

parseMemoryField :: Parser (Maybe MemoryField)
parseMemoryField =
  OA.optional
    $ OA.option
      (Memory.parseMemoryField OA.str)
      ( OA.long Memory.fieldKey
          <> OA.short 'f'
          <> OA.metavar "(free | total | used)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "If specified, prints only the given field. Default to total."

parseMemoryApp :: Parser (Maybe MemoryApp)
parseMemoryApp =
  OA.optional
    $ OA.option
      (Memory.parseMemoryApp OA.str)
      ( OA.long "app"
          <> OA.short 'a'
          <> OA.metavar "(free)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "Specifies the app to use."

parseMemoryUnits :: Parser (Maybe MemoryUnits)
parseMemoryUnits =
  OA.optional
    $ OA.option
      (Memory.parseMemoryUnits OA.str)
      ( mconcat
          [ OA.short 'u',
            OA.long Memory.unitsKey,
            OA.metavar "(bytes | percentage)",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt = "Units to use."

parseNetConn :: Parser PythiaCommand1
parseNetConn = NetConnCmd <$> parseApp <*> parseNetConnField
  where
    parseApp = netInterfaceApp

parseNetConnField :: Parser (Maybe NetConnField)
parseNetConnField =
  OA.optional
    $ OA.option
      (NetConn.parseNetConnField OA.str)
      ( OA.long NetConn.fieldKey
          <> OA.short 'f'
          <> OA.metavar "(device | type | name | ipv4 | ipv6)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "If specified, prints only the given field."

parseNetInterface :: Parser PythiaCommand1
parseNetInterface = do
  app <- netInterfaceApp
  device <- netInterfaceDevice
  val <- parseNetInterfaceField
  pure $ NetInterfaceCmd app device val

parseNetInterfaceField :: Parser (Maybe NetInterfaceField)
parseNetInterfaceField =
  OA.optional
    $ OA.option
      (NetInterface.parseNetInterfaceField OA.str)
      ( OA.long NetInterface.fieldKey
          <> OA.short 'f'
          <> OA.metavar "(name | ipv4 | ipv6)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "If specified, prints only the given field."

netInterfaceApp :: Parser (Maybe NetInterfaceApp)
netInterfaceApp =
  OA.optional
    $ OA.option
      (NetInterface.parseNetInterfaceApp OA.str)
      ( OA.long "app"
          <> OA.short 'a'
          <> OA.metavar "(ip | nmcli)"
          <> mkHelp helpTxt
      )
  where
    helpTxt = "The app to use."

netInterfaceDevice :: Parser (Maybe NetInterfaceDevice)
netInterfaceDevice =
  OA.optional
    $ OA.option
      (NetInterface.parseNetInterfaceDevice OA.str)
      ( OA.long NetInterface.deviceKey
          <> OA.short 'd'
          <> OA.metavar "(none | NAME)"
          <> mkHelp deviceTxt
      )
  where
    deviceTxt =
      mconcat
        [ "The name of the network device to filter on e.g. wlp0s20f3. The ",
          "string 'none' explicitly opts out of filtering (the default)."
        ]

parseTime :: Parser PythiaCommand1
parseTime = TimeCmd <$> parseTimezoneDest <*> parseTimeFormat

parseTimezoneDest :: Parser (Maybe TimezoneDest)
parseTimezoneDest =
  OA.optional
    $ OA.option
      (Time.parseTimeDest OA.str)
      ( OA.long Time.destKey
          <> OA.short 'd'
          <> OA.metavar "(local | utc | TZ)"
          <> mkHelp helpTxt
      )
  where
    helpTxt =
      mconcat
        [ "Determines what timezone we return. If none is given we assume",
          " local time. If given, must be one of [utc | TZ] where TZ is a tz",
          " database label e.g. America/New_York. See",
          " https://en.wikipedia.org/wiki/Tz_database."
        ]

parseTimeFormat :: Parser (Maybe TimeFormat)
parseTimeFormat =
  OA.optional
    $ OA.option
      (Time.parseTimeFormat OA.str)
      ( OA.long Time.formatKey
          <> OA.short 'f'
          <> OA.metavar "(default | FMT_STR)"
          <> mkHelp helpTxt
      )
  where
    helpTxt =
      mconcat
        [ "Glibc-style format string e.g. %Y-%m-%d for yyyy-mm-dd. ",
          "Defaults to RFC822. See ",
          "https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime. "
        ]

mkCommand :: String -> Parser a -> OA.InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkCmdDesc :: String -> OA.InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
