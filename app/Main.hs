-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Main (main) where

import Args
  ( BatteryField (..),
    NetConnField (..),
    NetInterfaceField (..),
    PythiaCommand (..),
    These (..),
    parserInfo,
  )
import Data.Text qualified as T
import Options.Applicative qualified as OApp
import Pythia
  ( BatteryConfig,
    Device,
    GlobalIpConfig (..),
    IpType (..),
    NetInterface (..),
    NetInterfaceConfig,
    NetInterfaces (..),
    UrlSource (..),
  )
import Pythia qualified
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Prelude

-- | Runs the executable.
--
-- @since 0.1
main :: IO ()
main = do
  cmd <- OApp.execParser parserInfo
  case cmd of
    BatteryCmd cfg field -> handleBattery cfg field
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
    toField BatteryFieldPercentage = Pythia.pretty . view #percentage
    toField BatteryFieldStatus = Pythia.pretty . view #status

handleNetInterface :: NetInterfaceConfig -> Maybe Device -> Maybe NetInterfaceField -> IO ()
handleNetInterface cfg mdevice field = do
  case mdevice of
    Nothing -> Pythia.queryNetInterfaces cfg >>= printInterfaces
    Just device -> Pythia.queryNetInterface device cfg >>= printInterface
  where
    printFn :: ((NetInterface -> Text) -> a -> b) -> (b -> Text) -> a -> IO ()
    printFn liftField toText =
      putStrLn
        . T.unpack
        . toText
        . liftField (maybe pretty toField field)

    printInterfaces :: NetInterfaces -> IO ()
    printInterfaces = printFn fmap (Pythia.joinX "\n\n") . unNetInterfaces

    printInterface :: NetInterface -> IO ()
    printInterface = printFn id id

    toField :: NetInterfaceField -> NetInterface -> Text
    toField NetInterfaceFieldName = Pythia.pretty . view #iname
    toField NetInterfaceFieldIpv4 = Pythia.joinCommas . view #ipv4s
    toField NetInterfaceFieldIpv6 = Pythia.joinCommas . view #ipv6s

handleNetConn :: NetInterfaceConfig -> Maybe NetConnField -> IO ()
handleNetConn cfg field = do
  result <- Pythia.queryNetInterfaces cfg
  let conn :: Maybe NetInterface
      conn = Pythia.findUp result
  case field of
    Nothing -> prettyPrint conn
    Just sel ->
      putStrLn $ T.unpack $ pretty $ fmap (toField sel) conn
  where
    toField :: NetConnField -> NetInterface -> Text
    toField NetConnFieldDevice = Pythia.pretty . view #idevice
    toField NetConnFieldType = Pythia.pretty . view #itype
    toField NetConnFieldName = Pythia.pretty . view #iname
    toField NetConnFieldIpv4 = Pythia.joinCommas . view #ipv4s
    toField NetConnFieldIpv6 = Pythia.joinCommas . view #ipv6s

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

prettyPrint :: PrettyPrinter a => a -> IO ()
prettyPrint = putStrLn . T.unpack . Pythia.pretty
