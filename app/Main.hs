-- | This modules provides an executable for querying system information.
--
-- @since 0.1.0.0
module Main (main) where

import Args
  ( BatteryField (..),
    NetInterfaceField (..),
    PythiaCommand (..),
    parserInfo,
  )
import Options.Applicative qualified as OApp
import Pythia (BatteryConfig, NetInterfaceConfig, NetInterfaces (..))
import Pythia qualified
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))

-- | Runs the executable.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  cmd <- OApp.execParser parserInfo
  case cmd of
    Battery cfg field -> handleBattery cfg field
    NetInterface cfg val -> handleNetInterface cfg val
    NetIpGlobal cfg -> Pythia.uncheckGlobalIp $ Pythia.queryGlobalIpConfig cfg >>= prettyPrint

handleBattery :: BatteryConfig -> Maybe BatteryField -> IO ()
handleBattery cfg mfield = do
  result <- Pythia.uncheckBattery $ Pythia.queryBatteryConfig cfg
  case mfield of
    Nothing -> prettyPrint result
    Just field -> putStrLn (toField field result)
  where
    toField BatteryFieldPercentage = Pythia.pretty . view #percentage
    toField BatteryFieldStatus = Pythia.pretty . view #status

handleNetInterface :: NetInterfaceConfig -> Maybe NetInterfaceField -> IO ()
handleNetInterface cfg val = do
  result <- Pythia.uncheckNetInterface $ Pythia.queryNetInterfacesConfig cfg
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

prettyPrint :: PrettyPrinter a => a -> IO ()
prettyPrint = putStrLn . Pythia.pretty
