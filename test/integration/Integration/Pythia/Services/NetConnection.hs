{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Pythia.Services.NetConnection (tests) where

import Data.List qualified as L
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
    "net-conn"
    [ testNetConnDefault,
      testNetConnName,
      testNetConnIpv4,
      testNetConnIpv6
    ]

testNetConnDefault :: TestTree
testNetConnDefault = testCase "default" $ do
  ipResults <- runIntIO ["net-conn", "-a", "ip"]
  assertOutput ipData ipResults

  nmcliResults <- runIntIO ["net-conn", "-a", "nmcli"]
  assertOutput nmcliData nmcliResults

testNetConnName :: TestTree
testNetConnName = testCase "name" $ do
  ipResults <- runIntIO ["net-conn", "-a", "ip", "-f", "name"]
  assertSingleOutput expectedIp ipResults

  nmcliResults <- runIntIO ["net-conn", "-a", "nmcli", "-f", "name"]
  assertSingleOutput expectedNmcli nmcliResults
  where
    expectedIp = "<nothing>"
    expectedNmcli = "SomeSSID"

testNetConnIpv4 :: TestTree
testNetConnIpv4 = testCase "ipv4" $ do
  ipResults <- runIntIO ["net-conn", "-a", "ip", "-f", "ipv4"]
  assertSingleOutput expectedIp ipResults

  nmcliResults <- runIntIO ["net-conn", "-a", "nmcli", "-f", "ipv4"]
  assertSingleOutput expectedNmcli nmcliResults
  where
    expectedIp = "192.168.1.2"
    expectedNmcli = "192.168.1.2"

testNetConnIpv6 :: TestTree
testNetConnIpv6 = testCase "Ipv6" $ do
  ipResults <- runIntIO ["net-conn", "-a", "ip", "-f", "ipv6"]
  assertSingleOutput expectedIp ipResults

  nmcliResults <- runIntIO ["net-conn", "-a", "nmcli", "-f", "ipv6"]
  assertSingleOutput expectedNmcli nmcliResults
  where
    expectedIp =
      "958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6"
    expectedNmcli =
      "958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6"

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
    "Shell command: ip address" ->
      let output =
            L.unlines
              [ "1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default qlen 1000",
                "link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00",
                "inet 127.0.0.1/8 scope host lo",
                "  valid_lft forever preferred_lft forever",
                "inet6 ::1/128 scope host noprefixroute ",
                "  valid_lft forever preferred_lft forever",
                "2: enp0s37f2: <NO-CARRIER,BROADCAST,MULTICAST,UP> mtu 1500 qdisc fq_codel state DOWN group default qlen 1000",
                "    link/ether 54:05:db:e1:b5:d8 brd ff:ff:ff:ff:ff:ff",
                "3: wlp0s15d2: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default qlen 1000",
                "    link/ether fc:44:82:de:f8:14 brd ff:ff:ff:ff:ff:ff",
                "    inet 192.168.1.2/24 brd 192.168.1.255 scope global dynamic noprefixroute wlp0s20f3",
                "      valid_lft 85634sec preferred_lft 74834sec",
                "    inet6 958a:d95d:75c8:2e2e:f802:21dd:4acc:908d/64 scope link noprefixroute ",
                "      valid_lft forever preferred_lft forever",
                "    inet6 958a::625a:a752:f875:329e:540b:27c6/64 scope link ",
                "      valid_lft forever preferred_lft forever",
                "4: tailscale0: <POINTOPOINT,MULTICAST,NOARP,UP,LOWER_UP> mtu 1280 qdisc fq_codel state UNKNOWN group default qlen 500",
                "    link/none ",
                "    inet6 c21b::9c97:63e0:cc8e::7516/64 scope link stable-privacy proto kernel_ll ",
                "      valid_lft forever preferred_lft forever"
              ]
       in pure (ExitSuccess, fromString output, "")
    "Shell command: nmcli -t -m multiline device show" ->
      let output =
            L.unlines
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
                "IP6.ROUTE[1]:dst = fe80::/64, nh = ::, mt = 256"
              ]
       in pure (ExitSuccess, fromString output, "")
    bad -> error $ "Unexpected command: " <> bad
    where
      cmd = processConfigToCmd pc

ipData :: [Text]
ipData =
  [ "Device: wlp0s15d2",
    "Type: <nothing>",
    "State: Up",
    "Name: <nothing>",
    "IPv4: 192.168.1.2",
    "IPv6: 958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6"
  ]

nmcliData :: [Text]
nmcliData =
  [ "Device: wlp0s15d2",
    "Type: Wifi",
    "State: Up",
    "Name: SomeSSID",
    "IPv4: 192.168.1.2",
    "IPv6: 958a:d95d:75c8:2e2e:f802:21dd:4acc:908d, 958a::625a:a752:f875:329e:540b:27c6"
  ]
