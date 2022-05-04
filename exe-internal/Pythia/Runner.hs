-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Pythia.Runner (runPythia) where

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

-- | Runs the executable.
--
-- @since 0.1
runPythia :: IO ()
runPythia = do
  cmd <- OApp.execParser parserInfo
  case cmd of
    BatteryCmd cfg field -> handleBattery cfg field
    MemoryCmd cfg field format -> handleMemory cfg field format
    NetInterfaceCmd cfg device field -> handleNetInterface cfg device field
    NetConnCmd cfg field -> handleNetConn cfg field
    NetIpGlobalCmd cfg -> handleGlobalIp cfg
    `catch` \(ex :: SomeException) -> putStrLn (displayException ex)

handleBattery :: BatteryConfig -> BatteryField -> IO ()
handleBattery cfg field =
  Pythia.queryBattery cfg
    >>= putStrLn . T.unpack . toField field
  where
    toField BatteryFieldDefault = U.prettyToText
    toField BatteryFieldPercentage = U.prettyToText . view #percentage
    toField BatteryFieldStatus = U.prettyToText . view #status

handleMemory :: MemoryConfig -> MemoryField -> MemoryFormat -> IO ()
handleMemory cfg field format =
  Pythia.queryMemory cfg
    >>= putStrLn . T.unpack . toField format field
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

handleNetInterface :: NetInterfaceConfig -> Maybe Device -> NetInterfaceField -> IO ()
handleNetInterface cfg mdevice field = do
  resultTxt <- case mdevice of
    Nothing -> interfacesToText <$> Pythia.queryNetInterfaces cfg
    Just device -> interfaceToText <$> Pythia.queryNetInterface device cfg
  putStrLn $ T.unpack resultTxt
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
    toField NetInterfaceFieldName = U.pretty . view #iname
    toField NetInterfaceFieldIpv4 = U.pretty . view #ipv4s
    toField NetInterfaceFieldIpv6 = U.pretty . view #ipv6s

handleNetConn :: NetInterfaceConfig -> NetConnField -> IO ()
handleNetConn cfg field = do
  result <- Pythia.queryNetInterfaces cfg
  putStrLn $ case Pythia.findUp result of
    Nothing -> "<No live connection found>"
    Just conn -> T.unpack $ toField field conn
  where
    toField :: NetConnField -> NetInterface -> Text
    toField NetConnFieldDefault = U.prettyToText
    toField NetConnFieldDevice = U.prettyToText . view #idevice
    toField NetConnFieldType = U.prettyToText . view #itype
    toField NetConnFieldName = U.prettyToText . view #iname
    toField NetConnFieldIpv4 = U.prettyToText . view #ipv4s
    toField NetConnFieldIpv6 = U.prettyToText . view #ipv6s

handleGlobalIp :: GlobalIpConfig (These [UrlSource 'Ipv4] [UrlSource 'Ipv6]) -> IO ()
handleGlobalIp cfg = do
  case cfg ^. #globalIpSources of
    This ipv4Sources ->
      Pythia.queryGlobalIpv4 (MkGlobalIpConfig (cfg ^. #globalIpApp) ipv4Sources)
        >>= prettyPrint
    That ipv6Sources ->
      Pythia.queryGlobalIpv6 (MkGlobalIpConfig (cfg ^. #globalIpApp) ipv6Sources)
        >>= prettyPrint
    These ipv4Sources ipv6Sources -> do
      (ipv4Address, ipv6Address) <-
        Pythia.queryGlobalIp $
          MkGlobalIpConfig
            (cfg ^. #globalIpApp)
            (ipv4Sources, ipv6Sources)

      prettyPrint ipv4Address
      prettyPrint ipv6Address

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . T.unpack . U.prettyToText

docToText :: Doc ann -> Text
docToText = U.renderStrict . U.layoutCompact
