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
import Effects.Exception (MonadGlobalException, setUncaughtExceptionHandler)
import Effects.Optparse (MonadOptparse)
import Effects.Optparse qualified as OApp
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
import Pythia.Data.Percentage (unPercentage)
import Pythia.Prelude
import Pythia.Services.Memory qualified as Mem

-- | Reads cli args and prints the results to stdout.
--
-- @since 0.1
runPythia ::
  ( MonadCatch m,
    MonadFileReader m,
    MonadGlobalException m,
    MonadPathReader m,
    MonadOptparse m,
    MonadTerminal m,
    MonadTime m,
    MonadTypedProcess m
  ) =>
  m ()
runPythia = runPythiaHandler putTextLn

-- | Reads cli args and applies the parameter handler.
--
-- @since 0.1
runPythiaHandler ::
  ( MonadCatch m,
    MonadFileReader m,
    MonadGlobalException m,
    MonadPathReader m,
    MonadOptparse m,
    MonadTerminal m,
    MonadTime m,
    MonadTypedProcess m
  ) =>
  (Text -> m a) ->
  m a
runPythiaHandler handler = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  OApp.execParser parserInfo >>= \case
    BatteryCmd cfg field -> handleBattery handler cfg field
    MemoryCmd cfg field format -> handleMemory handler cfg field format
    NetInterfaceCmd cfg device field -> handleNetInterface handler cfg device field
    NetConnCmd cfg field -> handleNetConn handler cfg field
    NetIpGlobalCmd cfg -> handleGlobalIp handler cfg
    TimeCmd ttype format -> handleTime handler format ttype

handleBattery ::
  ( MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadTypedProcess m
  ) =>
  (Text -> m a) ->
  BatteryApp ->
  BatteryField ->
  m a
handleBattery handler cfg field =
  Pythia.queryBattery cfg
    >>= handler
    . toField field
  where
    toField BatteryFieldDefault = display
    toField BatteryFieldPercentage = display . view #percentage
    toField BatteryFieldStatus = display . view #status

handleMemory ::
  ( MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  (Text -> m a) ->
  MemoryApp ->
  MemoryField ->
  MemoryFormat ->
  m a
handleMemory handler cfg field format =
  Pythia.queryMemory cfg
    >>= handler
    . toField format field
  where
    toField :: MemoryFormat -> MemoryField -> SystemMemory -> Text
    toField MemoryBytes MemoryFieldDefault = display
    toField MemoryBytes MemoryFieldTotal = display . view #total
    toField MemoryBytes MemoryFieldUsed = display . view #used
    toField MemoryBytes MemoryFieldFree = display . Mem.freeMemory
    -- so we don't have an extra %
    toField MemoryPercentage MemoryFieldDefault =
      (<> " / 100%")
        . T.pack
        . show
        . unPercentage
        . Mem.percentageUsed
    toField MemoryPercentage MemoryFieldTotal = const "100%"
    toField MemoryPercentage MemoryFieldUsed = display . Mem.percentageUsed
    toField MemoryPercentage MemoryFieldFree = display . Mem.percentageFree

handleNetInterface ::
  ( MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  (Text -> m a) ->
  NetInterfaceApp ->
  Maybe Device ->
  NetInterfaceField ->
  m a
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

handleNetConn ::
  ( MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  (Text -> m a) ->
  NetInterfaceApp ->
  NetConnField ->
  m a
handleNetConn handler cfg field = do
  result <- Pythia.queryNetInterfaces cfg
  handler $ case Pythia.findUp result of
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

handleGlobalIp ::
  ( MonadCatch m,
    MonadTypedProcess m
  ) =>
  (Text -> m a) ->
  GlobalIpConfig (These [UrlSource Ipv4] [UrlSource Ipv6]) ->
  m a
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
  ( MonadCatch m,
    MonadTime m
  ) =>
  (Text -> m a) ->
  Maybe String ->
  TimezoneDest ->
  m a
handleTime handler mformat = \case
  TimezoneDestLocal -> Pythia.queryLocalTime >>= handler . formatTime
  TimezoneDestUTC -> Pythia.queryUTC >>= handler . formatTime
  TimezoneDestTZ tz -> Pythia.queryTimeZone tz >>= handler . formatTime
  where
    format = fromMaybe Format.rfc822DateFormat mformat
    formatTime :: (FormatTime t) => t -> Text
    formatTime = T.pack . Format.formatTime Format.defaultTimeLocale format

prettyPrint :: (Display a) => (Text -> m b) -> a -> m b
prettyPrint handler = handler . display
