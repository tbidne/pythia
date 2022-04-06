-- | This modules provides an executable for querying system information.
--
-- @since 0.1.0.0
module Main (main) where

import Args
  ( BatteryField (..),
    NetConnField (..),
    NetInterfaceField (..),
    PythiaCommand (..),
    parserInfo,
  )
import Options.Applicative qualified as OApp
import Pythia
  ( BatteryConfig,
    NetInterface (..),
    NetInterfaceConfig,
    NetInterfaces (..),
    PythiaException,
  )
import Pythia qualified
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Prelude

-- | Runs the executable.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  cmd <- OApp.execParser parserInfo
  case cmd of
    BatteryCmd cfg field -> handleBattery cfg field
    NetInterfaceCmd cfg val -> handleNetInterface cfg val
    NetConnCmd cfg field -> handleNetConn cfg field
    NetIpGlobalCmd cfg -> Pythia.queryGlobalIpConfig cfg >>= prettyPrint
    `catch` \(ex :: PythiaException) -> putStrLn (displayException ex)

handleBattery :: BatteryConfig -> Maybe BatteryField -> IO ()
handleBattery cfg mfield = do
  result <- Pythia.queryBatteryConfig cfg
  case mfield of
    Nothing -> prettyPrint result
    Just field -> putStrLn (toField field result)
  where
    toField BatteryFieldPercentage = Pythia.pretty . view #percentage
    toField BatteryFieldStatus = Pythia.pretty . view #status

handleNetInterface :: NetInterfaceConfig -> Maybe NetInterfaceField -> IO ()
handleNetInterface cfg val = do
  result <- Pythia.queryNetInterfacesConfig cfg
  case val of
    Nothing -> prettyPrint result
    Just sel -> printField sel result
  where
    printField s =
      putStrLn
        . Pythia.joinNewlines
        . fmap (toField s)
        . unNetInterfaces

    toField NetInterfaceFieldName = Pythia.pretty . view #iname
    toField NetInterfaceFieldIpv4 = Pythia.joinCommas . view #ipv4s
    toField NetInterfaceFieldIpv6 = Pythia.joinCommas . view #ipv6s

handleNetConn :: NetInterfaceConfig -> Maybe NetConnField -> IO ()
handleNetConn cfg field = do
  result <- Pythia.queryNetInterfacesConfig cfg
  let conn :: Maybe NetInterface
      conn = Pythia.findUp result
  case field of
    Nothing -> prettyPrint conn
    Just sel ->
      putStrLn $ pretty $ fmap (toField sel) conn
  where
    toField :: NetConnField -> NetInterface -> String
    toField NetConnFieldDevice = Pythia.pretty . view #idevice
    toField NetConnFieldType = Pythia.pretty . view #itype
    toField NetConnFieldName = Pythia.pretty . view #iname
    toField NetConnFieldIpv4 = Pythia.joinCommas . view #ipv4s
    toField NetConnFieldIpv6 = Pythia.joinCommas . view #ipv6s

prettyPrint :: PrettyPrinter a => a -> IO ()
prettyPrint = putStrLn . Pythia.pretty
