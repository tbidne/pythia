module Unit.Pythia.Services.NetInterface.NmCli
  ( tests,
  )
where

import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Text qualified as T
import Pythia.Services.NetInterface.NmCli qualified as NmCli
import Pythia.Services.NetInterface.Types
  ( NetInterface (..),
    NetInterfaceState (..),
    NetInterfaceType (..),
    NetInterfaces (..),
  )
import Test.Tasty.HUnit qualified as THU
import Unit.Prelude

tests :: TestTree
tests = testGroup "Pythia.Services.NetInterface.NmCli" [parseAll]

parseAll :: TestTree
parseAll = testCase "Parses all interfaces" $ do
  let eResult = NmCli.parseInterfaces netinfo
  resultSet <- case eResult of
    Left ex -> THU.assertFailure $ "Parser failed in test: " <> show ex
    Right (MkNetInterfaces result) -> pure $ Set.fromList result

  let notFound = expectedSet \\ resultSet
      extras = resultSet \\ expectedSet

  if (not . Set.null) notFound
    then
      THU.assertFailure $
        "Did not find expected interface(s):"
          <> prettyInterfaces (Set.toList notFound)
          <> ".\n\nResult(s):"
          <> prettyInterfaces (Set.toList resultSet)
    else pure ()

  if (not . Set.null) extras
    then
      THU.assertFailure $
        "Found unexpected interface(s):"
          <> prettyInterfaces (Set.toList extras)
          <> ".\n\nExpected:"
          <> prettyInterfaces (Set.toList expectedSet)
    else pure ()
  where
    expectedSet =
      Set.fromList
        [ wifi,
          wifiP2p,
          ethernet,
          loopback,
          vpn
        ]

prettyInterfaces :: [NetInterface] -> String
prettyInterfaces [] = ""
prettyInterfaces (x : xs) = "\n\n" <> show x <> prettyInterfaces xs

wifi :: NetInterface
wifi =
  MkNetInterface
    "wlp0s20f3"
    (Just Wifi)
    Up
    (Just "KiWiFi")
    [unsafeIpAddress "192.168.1.2"]
    [unsafeIpAddress "fe80::fe44:82ff:fede:f814", unsafeIpAddress "fe80::a328:482:5263:10b8"]

wifiP2p :: NetInterface
wifiP2p =
  MkNetInterface
    "p2p-dev-wlp0s20f3"
    (Just Wifi_P2P)
    Down
    Nothing
    []
    []

ethernet :: NetInterface
ethernet =
  MkNetInterface
    "enp0s31f6"
    (Just Ethernet)
    Down
    Nothing
    []
    []

loopback :: NetInterface
loopback =
  MkNetInterface
    "lo"
    (Just Loopback)
    (UnknownState "(unmanaged)")
    Nothing
    [unsafeIpAddress "127.0.0.1"]
    [unsafeIpAddress "::1"]

vpn :: NetInterface
vpn =
  MkNetInterface
    "tailscale0"
    (Just Tun)
    (UnknownState "(unmanaged)")
    Nothing
    []
    [unsafeIpAddress "fe80::a63f:791a:3eaa:9d86"]

netinfo :: Text
netinfo =
  T.unlines
    [ "GENERAL.DEVICE:wlp0s20f3",
      "GENERAL.TYPE:wifi",
      "GENERAL.HWADDR:FC:44:82:DE:F8:14",
      "GENERAL.MTU:1500",
      "GENERAL.STATE:100 (connected)",
      "GENERAL.CONNECTION:KiWiFi",
      "GENERAL.CON-PATH:/org/freedesktop/NetworkManager/ActiveConnection/1",
      "IP4.ADDRESS[1]:192.168.1.2/24",
      "IP4.GATEWAY:192.168.1.1",
      "IP4.ROUTE[1]:dst = 192.168.1.0/24, nh = 0.0.0.0, mt = 3002",
      "IP4.ROUTE[2]:dst = 0.0.0.0/0, nh = 192.168.1.1, mt = 3002",
      "IP4.DNS[1]:192.168.1.1",
      "IP4.DOMAIN[1]:home",
      "IP6.ADDRESS[1]:fe80::fe44:82ff:fede:f814/64",
      "IP6.ADDRESS[2]:fe80::a328:482:5263:10b8/64",
      "IP6.GATEWAY:",
      "IP6.ROUTE[1]:dst = fe80::/64, nh = ::, mt = 1024",
      "IP6.ROUTE[2]:dst = fe80::/64, nh = ::, mt = 256",
      "",
      "GENERAL.DEVICE:p2p-dev-wlp0s20f3",
      "GENERAL.TYPE:wifi-p2p",
      "GENERAL.HWADDR:",
      "GENERAL.MTU:0",
      "GENERAL.STATE:30 (disconnected)",
      "GENERAL.CONNECTION:",
      "GENERAL.CON-PATH:",
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
      "GENERAL.DEVICE:enp0s31f6",
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
      "GENERAL.DEVICE:tailscale0",
      "GENERAL.TYPE:tun",
      "GENERAL.HWADDR:",
      "GENERAL.MTU:1280",
      "GENERAL.STATE:10 (unmanaged)",
      "GENERAL.CONNECTION:",
      "GENERAL.CON-PATH:",
      "IP4.GATEWAY:",
      "IP6.ADDRESS[1]:fe80::a63f:791a:3eaa:9d86/64",
      "IP6.GATEWAY:",
      "IP6.ROUTE[1]:dst = fe80::/64, nh = ::, mt = 256"
    ]
