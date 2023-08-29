-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Pythia.Runner
  ( runPythia,
    runPythiaHandler,
  )
where

import Data.Text qualified as T
import Data.Time.Format (FormatTime)
import Data.Time.Format qualified as Format
import Effectful.Optparse.Static (OptparseStatic)
import Effectful.Optparse.Static qualified as OA
import Effectful.Terminal.Static (TerminalStatic)
import Effectful.Terminal.Static qualified as Term
import Pythia
  ( BatteryApp,
    Device,
    GlobalIpConfig (..),
    IpType (..),
    MemoryApp (..),
    NetInterface (..),
    NetInterfaceApp,
    NetInterfaces (..),
    SystemMemory (..),
    UrlSource (..),
  )
import Pythia qualified
import Pythia.Args
  ( BatteryField (..),
    MemoryField (..),
    MemoryFormat (..),
    NetConnField (..),
    NetInterfaceField (..),
    PythiaCommand (..),
    These (..),
    TimezoneDest (..),
    parserInfo,
  )
import Pythia.Data.Percentage (rawPercentage)
import Pythia.Prelude
import Pythia.Services.Memory qualified as Mem
import Pythia.Utils (Doc, Pretty (..))
import Pythia.Utils qualified as U

-- | Reads cli args and prints the results to stdout.
--
-- @since 0.1
runPythia ::
  ( FileReaderDynamic :> es,
    OptparseStatic :> es,
    PathReaderDynamic :> es,
    TimeDynamic :> es,
    TerminalStatic :> es,
    TypedProcess :> es
  ) =>
  Eff es ()
runPythia = runPythiaHandler Term.putTextLn

-- | Reads cli args and applies the parameter handler.
--
-- @since 0.1
runPythiaHandler ::
  ( FileReaderDynamic :> es,
    OptparseStatic :> es,
    PathReaderDynamic :> es,
    TimeDynamic :> es,
    TypedProcess :> es
  ) =>
  (Text -> Eff es a) ->
  Eff es a
runPythiaHandler handler = do
  OA.execParser parserInfo >>= \case
    BatteryCmd cfg field -> handleBattery handler cfg field
    MemoryCmd cfg field format -> handleMemory handler cfg field format
    NetInterfaceCmd cfg device field -> handleNetInterface handler cfg device field
    NetConnCmd cfg field -> handleNetConn handler cfg field
    NetIpGlobalCmd cfg -> handleGlobalIp handler cfg
    TimeCmd ttype format -> handleTime handler format ttype

handleBattery ::
  ( FileReaderDynamic :> es,
    PathReaderDynamic :> es,
    TypedProcess :> es
  ) =>
  (Text -> Eff es a) ->
  BatteryApp ->
  BatteryField ->
  Eff es a
handleBattery handler cfg field =
  Pythia.queryBattery cfg
    >>= handler
    . toField field
  where
    toField BatteryFieldDefault = U.prettyToText
    toField BatteryFieldPercentage = U.prettyToText . view #percentage
    toField BatteryFieldStatus = U.prettyToText . view #status

handleMemory ::
  ( PathReaderDynamic :> es,
    TypedProcess :> es
  ) =>
  (Text -> Eff es a) ->
  MemoryApp ->
  MemoryField ->
  MemoryFormat ->
  Eff es a
handleMemory handler cfg field format =
  Pythia.queryMemory cfg
    >>= handler
    . toField format field
  where
    toField :: MemoryFormat -> MemoryField -> SystemMemory -> Text
    toField MemoryBytes MemoryFieldDefault = U.prettyToText
    toField MemoryBytes MemoryFieldTotal = U.prettyToText . view #total
    toField MemoryBytes MemoryFieldUsed = U.prettyToText . view #used
    toField MemoryBytes MemoryFieldFree = U.prettyToText . Mem.freeMemory
    -- so we don't have an extra %
    toField MemoryPercentage MemoryFieldDefault =
      (<> " / 100%")
        . T.pack
        . show
        . rawPercentage
        . Mem.percentageUsed
    toField MemoryPercentage MemoryFieldTotal = const "100%"
    toField MemoryPercentage MemoryFieldUsed = U.prettyToText . Mem.percentageUsed
    toField MemoryPercentage MemoryFieldFree = U.prettyToText . Mem.percentageFree

handleNetInterface ::
  ( PathReaderDynamic :> es,
    TypedProcess :> es
  ) =>
  (Text -> Eff es a) ->
  NetInterfaceApp ->
  Maybe Device ->
  NetInterfaceField ->
  Eff es a
handleNetInterface handler cfg mdevice field = do
  resultTxt <- case mdevice of
    Nothing -> interfacesToText <$> Pythia.queryNetInterfaces cfg
    Just device -> interfaceToText <$> Pythia.queryNetInterface device cfg
  handler resultTxt
  where
    interfacesToText :: NetInterfaces -> Text
    interfacesToText =
      case field of
        -- special case so we can use NetInterface's Pretty instance directly,
        -- which will print extra newlines between interfaces.
        NetInterfaceFieldDefault -> U.prettyToText
        _ ->
          docToText
            . U.vsep
            . fmap (toField field)
            . view #unNetInterfaces

    interfaceToText :: NetInterface -> Text
    interfaceToText = docToText . toField field

    toField :: NetInterfaceField -> NetInterface -> Doc ann
    toField NetInterfaceFieldDefault = U.pretty
    toField NetInterfaceFieldName = U.pretty . view #name
    toField NetInterfaceFieldIpv4 = U.pretty . view #ipv4s
    toField NetInterfaceFieldIpv6 = U.pretty . view #ipv6s

handleNetConn ::
  ( PathReaderDynamic :> es,
    TypedProcess :> es
  ) =>
  (Text -> Eff es a) ->
  NetInterfaceApp ->
  NetConnField ->
  Eff es a
handleNetConn handler cfg field = do
  result <- Pythia.queryNetInterfaces cfg
  handler $ case Pythia.findUp result of
    Nothing -> "<No live connection found>"
    Just conn -> toField field conn
  where
    toField :: NetConnField -> NetInterface -> Text
    toField NetConnFieldDefault = U.prettyToText
    toField NetConnFieldDevice = U.prettyToText . view #device
    toField NetConnFieldType = U.prettyToText . view #ntype
    toField NetConnFieldName = U.prettyToText . view #name
    toField NetConnFieldIpv4 = U.prettyToText . view #ipv4s
    toField NetConnFieldIpv6 = U.prettyToText . view #ipv6s

handleGlobalIp ::
  ( TypedProcess :> es
  ) =>
  (Text -> Eff es a) ->
  GlobalIpConfig (These [UrlSource Ipv4] [UrlSource Ipv6]) ->
  Eff es a
handleGlobalIp handler cfg = do
  case cfg ^. #sources of
    This ipv4Sources ->
      Pythia.queryGlobalIpv4 (MkGlobalIpConfig (cfg ^. #app) ipv4Sources)
        >>= prettyPrint handler
    That ipv6Sources ->
      Pythia.queryGlobalIpv6 (MkGlobalIpConfig (cfg ^. #app) ipv6Sources)
        >>= prettyPrint handler
    These ipv4Sources ipv6Sources -> do
      (ipv4Address, ipv6Address) <-
        Pythia.queryGlobalIp
          $ MkGlobalIpConfig
            (cfg ^. #app)
            (ipv4Sources, ipv6Sources)

      _ <- prettyPrint handler ipv4Address
      prettyPrint handler ipv6Address

handleTime ::
  ( TimeDynamic :> es
  ) =>
  (Text -> Eff es a) ->
  Maybe String ->
  TimezoneDest ->
  Eff es a
handleTime handler mformat = \case
  TimezoneDestLocal -> Pythia.queryLocalTime >>= handler . formatTime
  TimezoneDestUTC -> Pythia.queryUTC >>= handler . formatTime
  TimezoneDestTZ tz -> Pythia.queryTimeZone tz >>= handler . formatTime
  where
    format = fromMaybe Format.rfc822DateFormat mformat
    formatTime :: (FormatTime t) => t -> Text
    formatTime = T.pack . Format.formatTime Format.defaultTimeLocale format

prettyPrint :: (Pretty a) => (Text -> Eff es b) -> a -> Eff es b
prettyPrint handler = handler . U.prettyToText

docToText :: Doc ann -> Text
docToText = U.renderStrict . U.layoutCompact
