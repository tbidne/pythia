{-# LANGUAGE TemplateHaskell #-}

module Unit.Pythia.Services.Network.IP.Local.NmCli
  ( tests,
  )
where

import Data.Text qualified as T
import Pythia (QueryError (..))
import Pythia.Services.Network.IP.Local.NmCli qualified as NmCli
import Pythia.Services.Network.IP.Local.Types
  ( Ipv4 (..),
    Ipv6 (..),
    LocalIpAddresses (..),
    LocalIps (..),
  )
import Pythia.Services.Network.Types (Device (..))
import Refined qualified as R
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
      (NmCli.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "wifidev"
    expected =
      MkLocalIps device $
        MkLocalIpAddresses
          { ipv4s = [MkIpv4 $$(R.refineTH "192.168.1.2")],
            ipv6s =
              [ MkIpv6 $$(R.refineTH "fe80::fe44:82ff:fede:f814"),
                MkIpv6 $$(R.refineTH "fe80::a328:482:5263:10b8")
              ]
          }

parsesWifiP2P :: TestTree
parsesWifiP2P = testCase "Parses Wifi-P2P" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "p2p-dev-wifi"
    expected =
      MkLocalIps device $
        MkLocalIpAddresses
          { ipv4s = [],
            ipv6s = []
          }

parsesEthernet :: TestTree
parsesEthernet = testCase "Parses Ethernet" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "ethdev"
    expected =
      MkLocalIps device $
        MkLocalIpAddresses
          { ipv4s = [],
            ipv6s = []
          }

parsesLoopback :: TestTree
parsesLoopback = testCase "Parses Loopback" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "lo"
    expected =
      MkLocalIps device $
        MkLocalIpAddresses
          { ipv4s = [MkIpv4 $$(R.refineTH "127.0.0.1")],
            ipv6s = [MkIpv6 $$(R.refineTH "::1")]
          }

parsesTun :: TestTree
parsesTun = testCase "Parses Tun" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "vpn"
    expected =
      MkLocalIps device $
        MkLocalIpAddresses
          { ipv4s = [],
            ipv6s = [MkIpv6 $$(R.refineTH "fe80::8049:e2d1:66cd:acf4")]
          }

parseFails :: TestTree
parseFails = testCase "Does not find non-extant device" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (NmCli.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Left expected @=? result
  where
    device = MkDevice "not a real device"
    expected =
      MkQueryError
        { name = "Pythia.Services.Network.LocalIps.NmCli",
          short = "Parse error",
          long = "Device not found: not a real device"
        }

netinfo :: Text
netinfo =
  T.unlines
    [ "GENERAL.DEVICE:wifidev",
      "GENERAL.TYPE:wifi",
      "GENERAL.HWADDR:FC:44:82:DE:F8:14",
      "GENERAL.MTU:1500",
      "GENERAL.STATE:100 (connected)",
      "GENERAL.CONNECTION:KiWiFi",
      "GENERAL.CON-PATH:/org/freedesktop/NetworkManager/ActiveConnection/1",
      "IP4.ADDRESS[1]:192.168.1.2/24",
      "IP4.GATEWAY:192.168.1.1",
      "IP4.ROUTE[1]:dst = 192.168.1.0/24, nh = 0.0.0.0, mt = 3003",
      "IP4.ROUTE[2]:dst = 0.0.0.0/0, nh = 192.168.1.1, mt = 3003",
      "IP4.DNS[1]:192.168.1.1",
      "IP6.ADDRESS[1]:fe80::fe44:82ff:fede:f814/64",
      "IP6.ADDRESS[2]:fe80::a328:482:5263:10b8/64",
      "IP6.GATEWAY:",
      "IP6.ROUTE[1]:dst = fe80::/64, nh = ::, mt = 1024",
      "IP6.ROUTE[2]:dst = fe80::/64, nh = ::, mt = 256",
      "",
      "GENERAL.DEVICE:p2p-dev-wifi",
      "GENERAL.TYPE:wifi-p2p",
      "GENERAL.HWADDR:",
      "GENERAL.MTU:0",
      "GENERAL.STATE:30 (disconnected)",
      "GENERAL.CONNECTION:",
      "GENERAL.CON-PATH:",
      "",
      "GENERAL.DEVICE:ethdev",
      "GENERAL.TYPE:ethernet",
      "GENERAL.HWADDR:54:05:DB:E1:B5:D8",
      "GENERAL.MTU:1500",
      "GENERAL.STATE:20 (unavailable)",
      "GENERAL.CONNECTION:",
      "GENERAL.CON-PATH:",
      "WIRED-PROPERTIES.CARRIER:off",
      "IP4.GATEWAY:",
      "IP6.GATEWAY:",
      "",
      "GENERAL.DEVICE:lo",
      "GENERAL.TYPE:loopback",
      "GENERAL.HWADDR:00:00:00:00:00:00",
      "GENERAL.MTU:65536",
      "GENERAL.STATE:10 (unmanaged)",
      "GENERAL.CONNECTION:",
      "GENERAL.CON-PATH:",
      "IP4.ADDRESS[1]:127.0.0.1/8",
      "IP4.GATEWAY:",
      "IP6.ADDRESS[1]:::1/128",
      "IP6.GATEWAY:",
      "IP6.ROUTE[1]:dst = ::1/128, nh = ::, mt = 256",
      "",
      "GENERAL.DEVICE:vpn",
      "GENERAL.TYPE:tun",
      "GENERAL.HWADDR:",
      "GENERAL.MTU:1280",
      "GENERAL.STATE:10 (unmanaged)",
      "GENERAL.CONNECTION:",
      "GENERAL.CON-PATH:",
      "IP4.GATEWAY:",
      "IP6.ADDRESS[1]:fe80::8049:e2d1:66cd:acf4/64",
      "IP6.GATEWAY:",
      "IP6.ROUTE[1]:dst = fe80::/64, nh = ::, mt = 256"
    ]
