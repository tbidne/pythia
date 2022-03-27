{-# LANGUAGE TemplateHaskell #-}

module Unit.Pythia.Services.Network.IP.Local.IfConfig
  ( tests,
  )
where

import Data.Text qualified as T
import Pythia (QueryError (..))
import Pythia.Services.Network.IP.Local.IfConfig qualified as IfConfig
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
    "Pythia.Services.Network.Connection.IfConfig"
    [ parsesWifi,
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
      (IfConfig.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Right expected @=? result
  where
    device = MkDevice "wifidev"
    expected =
      MkLocalIps device $
        MkLocalIpAddresses
          { ipv4s = [MkIpv4 $$(R.refineTH "192.168.1.2")],
            ipv6s =
              [ MkIpv6 $$(R.refineTH "fe80::a328:482:5263:10b8"),
                MkIpv6 $$(R.refineTH "fe80::fe44:82ff:fede:f814")
              ]
          }

parsesEthernet :: TestTree
parsesEthernet = testCase "Parses Ethernet" $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (IfConfig.ipShellApp device ^? #_SimpleApp % #parser)
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
      (IfConfig.ipShellApp device ^? #_SimpleApp % #parser)
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
      (IfConfig.ipShellApp device ^? #_SimpleApp % #parser)
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
      (IfConfig.ipShellApp device ^? #_SimpleApp % #parser)
  let result = parser netinfo
  Left expected @=? result
  where
    device = MkDevice "not a real device"
    expected =
      MkQueryError
        { name = "Pythia.Services.Network.LocalIps.IfConfig",
          short = "Parse error",
          long = "Device not found: not a real device"
        }

netinfo :: Text
netinfo =
  T.unlines
    [ "ethdev: flags=4099<UP,BROADCAST,MULTICAST>  mtu 1500",
      "        ether 54:05:db:e1:b5:d8  txqueuelen 1000  (Ethernet)",
      "        RX packets 0  bytes 0 (0.0 B)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 0  bytes 0 (0.0 B)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0",
      "        device interrupt 16  memory 0xea200000-ea220000  ",
      "",
      "lo: flags=73<UP,LOOPBACK,RUNNING>  mtu 65536",
      "        inet 127.0.0.1  netmask 255.0.0.0",
      "        inet6 ::1  prefixlen 128  scopeid 0x10<host>",
      "        loop  txqueuelen 1000  (Local Loopback)",
      "        RX packets 2427  bytes 724057 (707.0 KiB)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 2427  bytes 724057 (707.0 KiB)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0",
      "",
      "vpn: flags=4305<UP,POINTOPOINT,RUNNING,NOARP,MULTICAST>  mtu 1280",
      "        inet6 fe80::8049:e2d1:66cd:acf4  prefixlen 64  scopeid 0x20<link>",
      "        unspec 00-00-00-00-00-00-00-00-00-00-00-00-00-00-00-00  txqueuelen 500  (UNSPEC)",
      "        RX packets 0  bytes 0 (0.0 B)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 14  bytes 672 (672.0 B)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0",
      "",
      "wifidev: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500",
      "        inet 192.168.1.2  netmask 255.255.255.0  broadcast 192.168.1.255",
      "        inet6 fe80::a328:482:5263:10b8  prefixlen 64  scopeid 0x20<link>",
      "        inet6 fe80::fe44:82ff:fede:f814  prefixlen 64  scopeid 0x20<link>",
      "        ether fc:44:82:de:f8:14  txqueuelen 1000  (Ethernet)",
      "        RX packets 35138  bytes 22102347 (21.0 MiB)",
      "        RX errors 0  dropped 0  overruns 0  frame 0",
      "        TX packets 25026  bytes 3790228 (3.6 MiB)",
      "        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0"
    ]
