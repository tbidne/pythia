module Unit.Pythia.Services.NetInterface.Ip
  ( tests,
  )
where

import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Text qualified as T
import Pythia.Services.NetInterface.Ip qualified as Ip
import Pythia.Services.NetInterface.Types
  ( NetInterface (..),
    NetInterfaceState (..),
    NetInterfaces (..),
  )
import Pythia.Services.Types.Network (IpAddresses (..))
import Test.Tasty.HUnit qualified as THU
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.NetInterface.Ip"
    [ parseAll
    ]

parseAll :: TestTree
parseAll = testCase "Parses all interfaces" $ do
  let eResult = Ip.parseInterfaces netinfo
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
    Nothing
    NetStateUp
    Nothing
    (MkIpAddresses [unsafeIpAddress "192.168.1.2"])
    (MkIpAddresses [unsafeIpAddress "fe80::a328:482:5263:10b8", unsafeIpAddress "fe80::fe44:82ff:fede:f814"])

ethernet :: NetInterface
ethernet =
  MkNetInterface
    "enp0s31f6"
    Nothing
    NetStateDown
    Nothing
    (MkIpAddresses [])
    (MkIpAddresses [])

loopback :: NetInterface
loopback =
  MkNetInterface
    "lo"
    Nothing
    (NetStateUnknown "UNKNOWN")
    Nothing
    (MkIpAddresses [unsafeIpAddress "127.0.0.1"])
    (MkIpAddresses [unsafeIpAddress "::1"])

vpn :: NetInterface
vpn =
  MkNetInterface
    "tailscale0"
    Nothing
    (NetStateUnknown "UNKNOWN")
    Nothing
    (MkIpAddresses [])
    (MkIpAddresses [unsafeIpAddress "fe80::a63f:791a:3eaa:9d86"])

netinfo :: Text
netinfo =
  T.unlines
    [ "1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default qlen 1000",
      "    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00",
      "    inet 127.0.0.1/8 scope host lo",
      "      valid_lft forever preferred_lft forever",
      "    inet6 ::1/128 scope host ",
      "      valid_lft forever preferred_lft forever",
      "2: wlp0s20f3: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default qlen 1000",
      "    link/ether fc:44:82:de:f8:14 brd ff:ff:ff:ff:ff:ff",
      "    inet 192.168.1.2/24 brd 192.168.1.255 scope global dynamic noprefixroute wlp0s20f3",
      "      valid_lft 69656sec preferred_lft 69656sec",
      "    inet6 fe80::a328:482:5263:10b8/64 scope link noprefixroute ",
      "      valid_lft forever preferred_lft forever",
      "    inet6 fe80::fe44:82ff:fede:f814/64 scope link ",
      "      valid_lft forever preferred_lft forever",
      "3: enp0s31f6: <NO-CARRIER,BROADCAST,MULTICAST,UP> mtu 1500 qdisc fq_codel state DOWN group default qlen 1000",
      "    link/ether 54:05:db:e1:b5:d8 brd ff:ff:ff:ff:ff:ff",
      "4: tailscale0: <POINTOPOINT,MULTICAST,NOARP,UP,LOWER_UP> mtu 1280 qdisc fq_codel state UNKNOWN group default qlen 500",
      "    link/none ",
      "    inet6 fe80::a63f:791a:3eaa:9d86/64 scope link stable-privacy ",
      "      valid_lft forever preferred_lft forever"
    ]
