-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Pythia.Runner
  ( runPythia,
    runPythiaHandler,
  )
where

import Data.Text qualified as T
import Options.Applicative qualified as OApp
import Pythia
  ( BatteryConfig,
    Device,
    GlobalIpConfig (..),
    IpType (..),
    MemoryConfig (..),
    NetInterface (..),
    NetInterfaceConfig,
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
runPythia :: IO ()
runPythia = runPythiaHandler (putStrLn . T.unpack)

-- | Reads cli args and applies the parameter handler.
--
-- @since 0.1
runPythiaHandler :: (Text -> IO a) -> IO a
runPythiaHandler handler = do
  cmd <- OApp.execParser parserInfo
  (\e -> handler (T.pack $ displayException e))
    `handleAny` case cmd of
      BatteryCmd cfg field -> handleBattery handler cfg field
      MemoryCmd cfg field format -> handleMemory handler cfg field format
      NetInterfaceCmd cfg device field -> handleNetInterface handler cfg device field
      NetConnCmd cfg field -> handleNetConn handler cfg field
      NetIpGlobalCmd cfg -> handleGlobalIp handler cfg

handleBattery :: (Text -> IO a) -> BatteryConfig -> BatteryField -> IO a
handleBattery handler cfg field =
  Pythia.queryBattery cfg
    >>= handler . toField field
  where
    toField BatteryFieldDefault = U.prettyToText
    toField BatteryFieldPercentage = U.prettyToText . view #percentage
    toField BatteryFieldStatus = U.prettyToText . view #status

handleMemory :: (Text -> IO a) -> MemoryConfig -> MemoryField -> MemoryFormat -> IO a
handleMemory handler cfg field format =
  Pythia.queryMemory cfg
    >>= handler . toField format field
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

handleNetInterface :: (Text -> IO a) -> NetInterfaceConfig -> Maybe Device -> NetInterfaceField -> IO a
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

handleNetConn :: (Text -> IO a) -> NetInterfaceConfig -> NetConnField -> IO a
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

handleGlobalIp :: (Text -> IO a) -> GlobalIpConfig (These [UrlSource 'Ipv4] [UrlSource 'Ipv6]) -> IO a
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
        Pythia.queryGlobalIp $
          MkGlobalIpConfig
            (cfg ^. #app)
            (ipv4Sources, ipv6Sources)

      _ <- prettyPrint handler ipv4Address
      prettyPrint handler ipv6Address

prettyPrint :: Pretty a => (Text -> IO b) -> a -> IO b
prettyPrint handler = handler . U.prettyToText

docToText :: Doc ann -> Text
docToText = U.renderStrict . U.layoutCompact
