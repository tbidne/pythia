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
import Pythia.Services.Types.Network (IpAddresses (..))
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

  unless (Set.null notFound)
    $ THU.assertFailure
    $ "Did not find expected interface(s):"
    <> prettyInterfaces (Set.toList notFound)
    <> ".\n\nResult(s):"
    <> prettyInterfaces (Set.toList resultSet)

  unless (Set.null extras)
    $ THU.assertFailure
    $ "Found unexpected interface(s):"
    <> prettyInterfaces (Set.toList extras)
    <> ".\n\nExpected:"
    <> prettyInterfaces (Set.toList expectedSet)
  where
    expectedSet =
      Set.fromList
        [ wifi,
          wifiP2p,
          ethernet,
          loopback,
          vpn,
          docker,
          unknown
        ]

prettyInterfaces :: [NetInterface] -> String
prettyInterfaces [] = ""
prettyInterfaces (x : xs) = "\n\n" <> show x <> prettyInterfaces xs

wifi :: NetInterface
wifi =
  MkNetInterface
    "wlp0s20f3"
    (Just Wifi)
    NetStateUp
    (Just "KiWiFi")
    (MkIpAddresses [unsafeIpAddress "192.168.1.2"])
    (MkIpAddresses [unsafeIpAddress "fe80::fe44:82ff:fede:f814", unsafeIpAddress "fe80::a328:482:5263:10b8"])

wifiP2p :: NetInterface
wifiP2p =
  MkNetInterface
    "p2p-dev-wlp0s20f3"
    (Just Wifi_P2P)
    NetStateDown
    Nothing
    mempty
    mempty

ethernet :: NetInterface
ethernet =
  MkNetInterface
    "enp0s31f6"
    (Just Ethernet)
    NetStateDown
    Nothing
    mempty
    mempty

loopback :: NetInterface
loopback =
  MkNetInterface
    "lo"
    (Just Loopback)
    (NetStateUnknown "(unmanaged)")
    Nothing
    (MkIpAddresses [unsafeIpAddress "127.0.0.1"])
    (MkIpAddresses [unsafeIpAddress "::1"])

docker :: NetInterface
docker =
  MkNetInterface
    "docker0"
    (Just Bridge)
    (NetStateUnknown "(connected (externally))")
    (Just "docker0")
    (MkIpAddresses [unsafeIpAddress "170.15.0.1"])
    mempty

vpn :: NetInterface
vpn =
  MkNetInterface
    "tailscale0"
    (Just Tun)
    (NetStateUnknown "(unmanaged)")
    Nothing
    mempty
    (MkIpAddresses [unsafeIpAddress "fe80::a63f:791a:3eaa:9d86"])

unknown :: NetInterface
unknown =
  MkNetInterface
    "some_device"
    (Just $ Unknown "foo")
    (NetStateUnknown "bar")
    (Just "meh")
    mempty
    mempty

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
      "IP6.ROUTE[1]:dst = fe80::/64, nh = ::, mt = 256",
      "",
      "GENERAL.DEVICE:docker0",
      "GENERAL.TYPE:bridge",
      "GENERAL.HWADDR:02:42:C3:F8:D5:49",
      "GENERAL.MTU:1500",
      "GENERAL.STATE:100 (connected (externally))",
      "GENERAL.CONNECTION:docker0",
      "GENERAL.CON-PATH:/org/freedesktop/NetworkManager/ActiveConnection/3",
      "IP4.ADDRESS[1]:170.15.0.1/16",
      "IP4.GATEWAY:",
      "IP4.ROUTE[1]:dst = 170.15.0.0/16, nh = 0.0.0.0, mt = 0",
      "IP6.GATEWAY:",
      "",
      "GENERAL.DEVICE:some_device",
      "GENERAL.TYPE:foo",
      "GENERAL.HWADDR:",
      "GENERAL.MTU:1500",
      "GENERAL.STATE:100 bar",
      "GENERAL.CONNECTION:meh",
      "GENERAL.CON-PATH:"
    ]
