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
    [ "[",
      "  {",
      "    \"ifindex\": 1,",
      "    \"ifname\": \"lo\",",
      "    \"flags\": [",
      "      \"LOOPBACK\",",
      "      \"UP\",",
      "      \"LOWER_UP\"",
      "    ],",
      "    \"mtu\": 65536,",
      "    \"qdisc\": \"noqueue\",",
      "    \"operstate\": \"UNKNOWN\",",
      "    \"group\": \"default\",",
      "    \"txqlen\": 1000,",
      "    \"link_type\": \"loopback\",",
      "    \"address\": \"00:00:00:00:00:00\",",
      "    \"broadcast\": \"00:00:00:00:00:00\",",
      "    \"addr_info\": [",
      "      {",
      "        \"family\": \"inet\",",
      "        \"local\": \"127.0.0.1\",",
      "        \"prefixlen\": 8,",
      "        \"broadcast\": \"172.17.255.255\",",
      "        \"scope\": \"host\",",
      "        \"label\": \"lo\",",
      "        \"valid_life_time\": 4294967295,",
      "        \"preferred_life_time\": 4294967295",
      "      },",
      "      {",
      "        \"family\": \"inet6\",",
      "        \"local\": \"::1\",",
      "        \"prefixlen\": 128,",
      "        \"scope\": \"host\",",
      "        \"noprefixroute\": true,",
      "        \"valid_life_time\": 4294967295,",
      "        \"preferred_life_time\": 4294967295",
      "      }",
      "    ]",
      "  },",
      "  {",
      "    \"ifindex\": 2,",
      "    \"ifname\": \"wlp0s20f3\",",
      "    \"flags\": [",
      "      \"BROADCAST\",",
      "      \"MULTICAST\",",
      "      \"UP\",",
      "      \"LOWER_UP\"",
      "    ],",
      "    \"mtu\": 1500,",
      "    \"qdisc\": \"noqueue\",",
      "    \"operstate\": \"UP\",",
      "    \"group\": \"default\",",
      "    \"txqlen\": 1000,",
      "    \"link_type\": \"ether\",",
      "    \"address\": \"fc:44:82:de:f8:14\",",
      "    \"broadcast\": \"ff:ff:ff:ff:ff:ff\",",
      "    \"addr_info\": [",
      "      {",
      "        \"family\": \"inet\",",
      "        \"local\": \"192.168.1.2\",",
      "        \"prefixlen\": 24,",
      "        \"broadcast\": \"192.168.1.255\",",
      "        \"scope\": \"global\",",
      "        \"dynamic\": true,",
      "        \"noprefixroute\": true,",
      "        \"label\": \"wlp0s20f3\",",
      "        \"valid_life_time\": 69656,",
      "        \"preferred_life_time\": 69656",
      "      },",
      "      {",
      "        \"family\": \"inet6\",",
      "        \"local\": \"fe80::a328:482:5263:10b8\",",
      "        \"prefixlen\": 64,",
      "        \"scope\": \"link\",",
      "        \"noprefixroute\": true,",
      "        \"valid_life_time\": 4294967295,",
      "        \"preferred_life_time\": 4294967295",
      "      },",
      "      {",
      "        \"family\": \"inet6\",",
      "        \"local\": \"fe80::fe44:82ff:fede:f814\",",
      "        \"prefixlen\": 64,",
      "        \"scope\": \"link\",",
      "        \"noprefixroute\": true,",
      "        \"valid_life_time\": 4294967295,",
      "        \"preferred_life_time\": 4294967295",
      "      }",
      "    ]",
      "  },",
      "  {",
      "    \"ifindex\": 3,",
      "    \"ifname\": \"enp0s31f6\",",
      "    \"flags\": [",
      "      \"NO-CARRIER\",",
      "      \"BROADCAST\",",
      "      \"MULTICAST\",",
      "      \"UP\"",
      "    ],",
      "    \"mtu\": 1500,",
      "    \"qdisc\": \"fq_codel\",",
      "    \"operstate\": \"DOWN\",",
      "    \"group\": \"default\",",
      "    \"txqlen\": 1000,",
      "    \"link_type\": \"ether\",",
      "    \"address\": \"54:05:db:e1:b5:d8\",",
      "    \"broadcast\": \"ff:ff:ff:ff:ff:ff\",",
      "    \"addr_info\": []",
      "  },",
      "  {",
      "    \"ifindex\": 4,",
      "    \"ifname\": \"tailscale0\",",
      "    \"flags\": [",
      "      \"POINTOPOINT\",",
      "      \"MULTICAST\",",
      "      \"NOARP\",",
      "      \"LOWER_UP\",",
      "      \"UP\"",
      "    ],",
      "    \"mtu\": 1280,",
      "    \"qdisc\": \"fq_codel\",",
      "    \"operstate\": \"UNKNOWN\",",
      "    \"group\": \"default\",",
      "    \"txqlen\": 500,",
      "    \"link_type\": \"none\",",
      "    \"address\": \"54:05:db:e1:b5:d8\",",
      "    \"broadcast\": \"ff:ff:ff:ff:ff:ff\",",
      "    \"addr_info\": [",
      "      {",
      "        \"family\": \"inet6\",",
      "        \"local\": \"fe80::a63f:791a:3eaa:9d86\",",
      "        \"prefixlen\": 64,",
      "        \"scope\": \"link\",",
      "        \"stable-privacy\": true,",
      "        \"valid_life_time\": 4294967295,",
      "        \"preferred_life_time\": 4294967295",
      "      }",
      "    ]",
      "  }",
      "]"
    ]
