{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for querying system information.
--
-- @since 0.1.0.0
module Main (main) where

import Args (NetInterfaceSelector (..), PythiaCommand (..), parserInfo)
import Options.Applicative qualified as OApp
import Pythia (NetInterface (..), NetInterfaceConfig, NetInterfaces (..))
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
    Battery cfg -> Pythia.uncheckBattery $ Pythia.queryBatteryConfig cfg >>= prettyPrint
    NetInterface cfg val -> handleNetInterface cfg val
    NetIpGlobal cfg -> Pythia.uncheckGlobalIp $ Pythia.queryGlobalIpConfig cfg >>= prettyPrint

handleNetInterface :: NetInterfaceConfig -> Maybe NetInterfaceSelector -> IO ()
handleNetInterface cfg val = do
  result <- Pythia.uncheckNetInterface $ Pythia.queryNetInterfacesConfig cfg
  case val of
    Nothing -> prettyPrint result
    Just sel -> printField sel result
  where
    printField :: NetInterfaceSelector -> NetInterfaces -> IO ()
    printField s =
      putStrLn
        . Pythia.joinNewlines
        . fmap (toField s)
        . unNetInterfaces

    toField :: NetInterfaceSelector -> NetInterface -> String
    toField NetInterfaceSelectorName netif = Pythia.pretty $ netif ^. #iname
    toField NetInterfaceSelectorIpv4 netif = Pythia.joinCommas $ netif ^. #ipv4s
    toField NetInterfaceSelectorIpv6 netif = Pythia.joinCommas $ netif ^. #ipv6s

prettyPrint :: PrettyPrinter a => a -> IO ()
prettyPrint = putStrLn . Pythia.pretty
