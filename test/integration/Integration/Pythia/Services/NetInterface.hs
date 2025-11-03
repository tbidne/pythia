{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Pythia.Services.NetInterface (tests) where

import Data.Text qualified as T
import Effects.FileSystem.PathReader (MonadPathReader (findExecutable))
import Effects.Optparse (MonadOptparse)
import Effects.Process.Typed
  ( ExitCode (ExitSuccess),
    MonadTypedProcess (readProcess),
  )
import Effects.System.Environment (MonadEnv)
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "net-if"
    [ testNetIfDefault,
      testNetIfName,
      testNetIfIpv4,
      testNetIfIpv6
    ]

testNetIfDefault :: TestTree
testNetIfDefault = testCase "default" $ do
  ipResults <- runIntIO ["net-if", "-a", "ip"]
  assertOutput ipData ipResults

  nmcliResults <- runIntIO ["net-if", "-a", "nmcli"]
  assertOutput nmcliData nmcliResults

testNetIfName :: TestTree
testNetIfName = testCase "name" $ do
  ipResults <- runIntIO ["net-if", "-a", "ip", "-f", "name"]
  assertOutput expectedIp ipResults

  nmcliResults <- runIntIO ["net-if", "-a", "nmcli", "-f", "name"]
  assertOutput expectedNmcli nmcliResults
  where
    expectedIp = ["<nothing>", "<nothing>", "<nothing>", "<nothing>"]
    expectedNmcli = ["SomeSSID", "lo", "<nothing>", "<nothing>", "<nothing>", "docker0"]

testNetIfIpv4 :: TestTree
testNetIfIpv4 = testCase "ipv4" $ do
  ipResults <- runIntIO ["net-if", "-a", "ip", "-f", "ipv4"]
  assertOutput expectedIp ipResults

  nmcliResults <- runIntIO ["net-if", "-a", "nmcli", "-f", "ipv4"]
  assertOutput expectedNmcli nmcliResults
  where
    expectedIp = ["127.0.0.1", "<empty>", "192.168.1.2", "<empty>"]
    expectedNmcli = ["192.168.1.2", "127.0.0.1", "<empty>", "<empty>", "<empty>", "170.15.0.1"]

testNetIfIpv6 :: TestTree
testNetIfIpv6 = testCase "Ipv6" $ do
  ipResults <- runIntIO ["net-if", "-a", "ip", "-f", "ipv6"]
  assertOutput expectedIp ipResults

  nmcliResults <- runIntIO ["net-if", "-a", "nmcli", "-f", "ipv6"]
  assertOutput expectedNmcli nmcliResults
  where
    expectedIp =
      [ "::1",
        "<empty>",
        "958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6",
        "c21b::9c97:63e0:cc8e::7516"
      ]
    expectedNmcli =
      [ "958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6",
        "::1",
        "<empty>",
        "<empty>",
        "fe80::dc17:70f2:696b:b31c",
        "<empty>"
      ]

runIntIO :: [String] -> IO [Text]
runIntIO = runIntegrationIO unIntIO

newtype IntIO a = MkIntIO {unIntIO :: ReaderT (IORef Text) IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadIO,
      MonadOptparse,
      MonadTime,
      MonadThrow
    )
    via ReaderT (IORef Text) IO
  deriving (MonadTerminal) via BaseIO

instance MonadFileReader IntIO

instance MonadPathReader IntIO where
  doesDirectoryExist _ = pure False
  getXdgDirectory _ _ = pure [osp|test_xdg|]

  findExecutable p
    | p == [osp|ip|] = pure $ Just [osp|exe|]
    | p == [osp|nmcli|] = pure $ Just [osp|exe|]
    | otherwise = pure Nothing

instance MonadTypedProcess IntIO where
  readProcess pc = case cmd of
    "Shell command: ip --json address" ->
      let output = ipNetInfo
       in pure (ExitSuccess, fromString (T.unpack output), "")
    "Shell command: nmcli -t -m multiline device show" ->
      let output =
            T.unlines
              [ "GENERAL.DEVICE:wlp0s15d2",
                "GENERAL.TYPE:wifi",
                "GENERAL.HWADDR:FC:44:82:DE:F8:14",
                "GENERAL.MTU:1500",
                "GENERAL.STATE:100 (connected)",
                "GENERAL.CONNECTION:SomeSSID",
                "GENERAL.CON-PATH:/org/freedesktop/NetworkManager/ActiveConnection/2",
                "IP4.ADDRESS[1]:192.168.1.2/24",
                "IP4.GATEWAY:192.168.1.1",
                "IP4.ROUTE[1]:dst = 192.168.1.0/24, nh = 0.0.0.0, mt = 3003",
                "IP4.ROUTE[2]:dst = 0.0.0.0/0, nh = 192.168.1.1, mt = 3003",
                "IP4.DNS[1]:192.168.1.1",
                "IP6.ADDRESS[1]:958a:d95d:75c8:2e2e:f802:21dd:4acc:908d/64",
                "IP6.ADDRESS[2]:958a::625a:a752:f875:329e:540b:27c6/64",
                "IP6.GATEWAY:",
                "IP6.ROUTE[1]:dst = 958a::/64, nh = ::, mt = 256",
                "IP6.ROUTE[2]:dst = 958a::/64, nh = ::, mt = 1024",
                "",
                "GENERAL.DEVICE:lo",
                "GENERAL.TYPE:loopback",
                "GENERAL.HWADDR:00:00:00:00:00:00",
                "GENERAL.MTU:65536",
                "GENERAL.STATE:100 (connected (externally))",
                "GENERAL.CONNECTION:lo",
                "GENERAL.CON-PATH:/org/freedesktop/NetworkManager/ActiveConnection/1",
                "IP4.ADDRESS[1]:127.0.0.1/8",
                "IP4.GATEWAY:",
                "IP6.ADDRESS[1]:::1/128",
                "IP6.GATEWAY:",
                "",
                "GENERAL.DEVICE:p2p-dev-wlp0s15d2",
                "GENERAL.TYPE:wifi-p2p",
                "GENERAL.HWADDR:",
                "GENERAL.MTU:0",
                "GENERAL.STATE:30 (disconnected)",
                "GENERAL.CONNECTION:",
                "GENERAL.CON-PATH:",
                "",
                "GENERAL.DEVICE:enp0s37f2",
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
                "IP6.ADDRESS[1]:fe80::dc17:70f2:696b:b31c/64",
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
                "IP6.GATEWAY:"
              ]
       in pure (ExitSuccess, fromString (T.unpack output), "")
    bad -> error $ "Unexpected command: " <> bad
    where
      cmd = processConfigToCmd pc

ipData :: [Text]
ipData =
  [ "Device: lo",
    "Type: <nothing>",
    "State: Unknown: UNKNOWN",
    "Name: <nothing>",
    "IPv4: 127.0.0.1",
    "IPv6: ::1",
    "",
    "Device: enp0s37f2",
    "Type: <nothing>",
    "State: Down",
    "Name: <nothing>",
    "IPv4: <empty>",
    "IPv6: <empty>",
    "",
    "Device: wlp0s15d2",
    "Type: <nothing>",
    "State: Up",
    "Name: <nothing>",
    "IPv4: 192.168.1.2",
    "IPv6: 958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6",
    "",
    "Device: tailscale0",
    "Type: <nothing>",
    "State: Unknown: UNKNOWN",
    "Name: <nothing>",
    "IPv4: <empty>",
    "IPv6: c21b::9c97:63e0:cc8e::7516"
  ]

nmcliData :: [Text]
nmcliData =
  [ "Device: wlp0s15d2",
    "Type: Wifi",
    "State: Up",
    "Name: SomeSSID",
    "IPv4: 192.168.1.2",
    "IPv6: 958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6",
    "",
    "Device: lo",
    "Type: Loopback",
    "State: Unknown: (connected (externally))",
    "Name: lo",
    "IPv4: 127.0.0.1",
    "IPv6: ::1",
    "",
    "Device: p2p-dev-wlp0s15d2",
    "Type: Wifi_P2P",
    "State: Down",
    "Name: <nothing>",
    "IPv4: <empty>",
    "IPv6: <empty>",
    "",
    "Device: enp0s37f2",
    "Type: Ethernet",
    "State: Down",
    "Name: <nothing>",
    "IPv4: <empty>",
    "IPv6: <empty>",
    "",
    "Device: tailscale0",
    "Type: Tun",
    "State: Unknown: (unmanaged)",
    "Name: <nothing>",
    "IPv4: <empty>",
    "IPv6: fe80::dc17:70f2:696b:b31c",
    "",
    "Device: docker0",
    "Type: Bridge",
    "State: Unknown: (connected (externally))",
    "Name: docker0",
    "IPv4: 170.15.0.1",
    "IPv6: <empty>"
  ]

ipNetInfo :: Text
ipNetInfo =
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
      "    \"ifname\": \"enp0s37f2\",",
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
      "    \"ifindex\": 3,",
      "    \"ifname\": \"wlp0s15d2\",",
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
      "        \"local\": \"958a:d95d:75c8:2e2e:f802:21dd:4acc:908d\",",
      "        \"prefixlen\": 64,",
      "        \"scope\": \"link\",",
      "        \"noprefixroute\": true,",
      "        \"valid_life_time\": 4294967295,",
      "        \"preferred_life_time\": 4294967295",
      "      },",
      "      {",
      "        \"family\": \"inet6\",",
      "        \"local\": \"958a::625a:a752:f875:329e:540b:27c6\",",
      "        \"prefixlen\": 64,",
      "        \"scope\": \"link\",",
      "        \"noprefixroute\": true,",
      "        \"valid_life_time\": 4294967295,",
      "        \"preferred_life_time\": 4294967295",
      "      }",
      "    ]",
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
      "        \"local\": \"c21b::9c97:63e0:cc8e::7516\",",
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
