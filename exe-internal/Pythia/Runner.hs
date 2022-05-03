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

handleBattery :: BatteryConfig -> Maybe BatteryField -> IO ()
handleBattery cfg mfield = do
  result <- Pythia.queryBattery cfg
  case mfield of
    Nothing -> prettyPrint result
    Just field -> putStrLn $ T.unpack (toField field result)
  where
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

handleNetInterface :: NetInterfaceConfig -> Maybe Device -> Maybe NetInterfaceField -> IO ()
handleNetInterface cfg mdevice mfield = do
  case mdevice of
    Nothing -> Pythia.queryNetInterfaces cfg >>= printInterfaces
    Just device -> Pythia.queryNetInterface device cfg >>= printInterface
  where
    printInterfaces :: NetInterfaces -> IO ()
    printInterfaces netifs@(MkNetInterfaces ifs) = case mfield of
      Nothing -> putStrLn $ T.unpack $ U.prettyToText netifs
      Just field -> putStrLn $ T.unpack $ docToText $ U.vsep $ fmap (toField field) ifs

    printInterface :: NetInterface -> IO ()
    printInterface inf = case mfield of
      Nothing -> putStrLn $ T.unpack $ U.prettyToText inf
      Just field -> putStrLn $ T.unpack $ docToText $ toField field inf

    toField NetInterfaceFieldName = U.pretty . view #iname
    toField NetInterfaceFieldIpv4 = U.pretty . view #ipv4s
    toField NetInterfaceFieldIpv6 = U.pretty . view #ipv6s

handleNetConn :: NetInterfaceConfig -> Maybe NetConnField -> IO ()
handleNetConn cfg field = do
  result <- Pythia.queryNetInterfaces cfg
  let conn :: Maybe NetInterface
      conn = Pythia.findUp result
  case field of
    Nothing -> prettyPrint conn
    Just sel ->
      putStrLn $ T.unpack $ U.prettyToText $ fmap (toField sel) conn
  where
    toField :: NetConnField -> NetInterface -> Text
    toField NetConnFieldDevice = U.prettyToText . view #idevice
    toField NetConnFieldType = U.prettyToText . view #itype
    toField NetConnFieldName = U.prettyToText . view #iname
    toField NetConnFieldIpv4 = docToText . U.pretty . view #ipv4s
    toField NetConnFieldIpv6 = docToText . U.pretty . view #ipv6s

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
