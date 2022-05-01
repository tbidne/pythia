-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Main (main) where

import Args
  ( BatteryField (..),
    MemoryField (..),
    NetConnField (..),
    NetInterfaceField (..),
    PythiaCommand (..),
    These (..),
    parserInfo,
  )
import Data.Bytes qualified as Bytes
import Data.Text qualified as T
import Numeric.Data.NonNegative qualified as NN
import Options.Applicative qualified as OApp
import Pythia
  ( BatteryConfig,
    Device,
    GlobalIpConfig (..),
    IpType (..),
    Memory (..),
    MemoryConfig (..),
    NetInterface (..),
    NetInterfaceConfig,
    NetInterfaces (..),
    UrlSource (..),
  )
import Pythia qualified
import Pythia.Prelude
import Pythia.Utils (Doc, Pretty (..))
import Pythia.Utils qualified as U

-- | Runs the executable.
--
-- @since 0.1
main :: IO ()
main = do
  cmd <- OApp.execParser parserInfo
  case cmd of
    BatteryCmd cfg field -> handleBattery cfg field
    MemoryCmd cfg field -> handleMemory cfg field
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

handleMemory :: MemoryConfig -> Maybe MemoryField -> IO ()
handleMemory cfg mfield = do
  result <- Pythia.queryMemory cfg
  putStrLn $ T.unpack $ prettyMem result
  where
    prettyMem :: Memory -> Text
    prettyMem = maybe U.prettyToText toField mfield

    toField :: MemoryField -> Memory -> Text
    toField MemoryFieldTotal m = toTxt #total m
    toField MemoryFieldUsed m = toTxt #used m
    toField MemoryFieldFree m = U.prettyToText $ t - u
      where
        t = NN.unNonNegative $ Bytes.unBytes $ m ^. #total
        u = NN.unNonNegative $ Bytes.unBytes $ m ^. #used

    toTxt getter = U.prettyToText . view getter

handleNetInterface :: NetInterfaceConfig -> Maybe Device -> Maybe NetInterfaceField -> IO ()
-- handleNetInterface cfg mdevice field = do
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
