module Unit.Pythia.Services.Network.Connection.NmCli
  ( tests,
  )
where

import Data.Text qualified as T
import Pythia.Data (QueryError (..))
import Pythia.Services.Network.Connection.NmCli qualified as NmCli
import Pythia.Services.Network.Connection.Types (ConnState (..), ConnType (..), Connection (..))
import Pythia.Services.Network.Types (Device (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Network.Connection.NmCli"
    [ parsesWifi,
      parsesWifiP2P,
      parsesEthernet,
      parsesLoopback,
      parsesTun,
      parseFails
    ]

parsesWifi :: TestTree
parsesWifi = testCase "Parses Wifi" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.connectionShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "device1"
    expected =
      MkConnection
        { connDevice = device,
          connType = Wifi,
          connState = Connected,
          connName = Just "SomeSSID"
        }

parsesWifiP2P :: TestTree
parsesWifiP2P = testCase "Parses Wifi-P2P" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.connectionShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "wifip2p"
    expected =
      MkConnection
        { connDevice = device,
          connType = Wifi_P2P,
          connState = Disconnected,
          connName = Nothing
        }

parsesEthernet :: TestTree
parsesEthernet = testCase "Parses Ethernet" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.connectionShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "ethdev"
    expected =
      MkConnection
        { connDevice = device,
          connType = Ethernet,
          connState = Unavailable,
          connName = Nothing
        }

parsesLoopback :: TestTree
parsesLoopback = testCase "Parses Loopback" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.connectionShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "lo"
    expected =
      MkConnection
        { connDevice = device,
          connType = Loopback,
          connState = Unmanaged,
          connName = Nothing
        }

parsesTun :: TestTree
parsesTun = testCase "Parses Tun" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.connectionShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "vpn"
    expected =
      MkConnection
        { connDevice = device,
          connType = Tun,
          connState = Unmanaged,
          connName = Nothing
        }

parseFails :: TestTree
parseFails = testCase "Does not find non-extant device" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.connectionShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Left expected @=? result
  where
    expected =
      MkQueryError
        { name = "Pythia.Services.Network.Connection.NmCli",
          short = "Parse error",
          long = "Could not find device `not a real device` in devices: , device1, wifip2p, ethdev, lo, vpn"
        }
    device = MkDevice "not a real device"

netinfo :: Text
netinfo =
  T.unlines
    [ "DEVICE:                                 device1",
      "TYPE:                                   wifi",
      "STATE:                                  connected",
      "CONNECTION:                             SomeSSID",
      "DEVICE:                                 wifip2p",
      "TYPE:                                   wifi-p2p",
      "STATE:                                  disconnected",
      "CONNECTION:                             --",
      "DEVICE:                                 ethdev",
      "TYPE:                                   ethernet",
      "STATE:                                  unavailable",
      "CONNECTION:                             --",
      "DEVICE:                                 lo",
      "TYPE:                                   loopback",
      "STATE:                                  unmanaged",
      "CONNECTION:                             --",
      "DEVICE:                                 vpn",
      "TYPE:                                   tun",
      "STATE:                                  unmanaged",
      "CONNECTION:                             --"
    ]
