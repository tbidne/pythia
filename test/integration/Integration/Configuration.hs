{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Configuration (tests) where

import Control.Exception (try)
import Control.Monad.Reader
import Effects.FileSystem.PathReader
  ( MonadPathReader (doesFileExist),
    XdgDirectory (XdgConfig),
  )
import Effects.Optparse (MonadOptparse)
import Effects.System.Environment (MonadEnv (withArgs))
import Integration.Prelude
import Pythia
  ( BatteryApp (BatteryAppSysFs, BatteryAppUPower),
    MemoryApp (MemoryAppFree),
    NetInterfaceApp (NetInterfaceAppIp, NetInterfaceAppNmCli),
  )
import Pythia.Runner qualified as Runner
import Pythia.Runner.Command
  ( PythiaCommand2,
    PythiaCommandP
      ( BatteryCmd,
        GlobalIpCmd,
        MemoryCmd,
        NetConnCmd,
        NetInterfaceCmd,
        TimeCmd
      ),
  )
import Pythia.Runner.Command.Battery
  ( BatteryField (BatteryFieldDefault, BatteryFieldPercentage, BatteryFieldStatus),
  )
import Pythia.Runner.Command.Memory
  ( MemoryField (MemoryFieldTotal, MemoryFieldUsed),
    MemoryUnits (MemoryUnitsBytes, MemoryUnitsPercentage),
  )
import Pythia.Runner.Command.NetConn
  ( NetConnField (NetConnFieldDefault, NetConnFieldIpv4),
  )
import Pythia.Runner.Command.NetInterface
  ( NetInterfaceDevice (NetInterfaceDeviceNone),
    NetInterfaceField (NetInterfaceFieldDefault, NetInterfaceFieldName),
  )
import Pythia.Runner.Command.Time
  ( TimeFormat (TimeFormatDefault),
    TimezoneDest (TimezoneDestTZ, TimezoneDestUTC),
  )
import Pythia.Runner.These (These (These, This))
import Pythia.Runner.Toml (ConfigException)
import Pythia.Services.GlobalIp (GlobalIpApp (GlobalIpAppCurl, GlobalIpAppDig))

tests :: TestTree
tests =
  testGroup
    "Configuration"
    [ tomlTests,
      argsTests,
      testMissingApps,
      testMisc
    ]

tomlTests :: TestTree
tomlTests =
  testGroup
    "Reads Toml"
    [ testBatteryToml,
      testGlobalIpToml,
      testMemoryToml,
      testNetConnToml,
      testNetInterfaceToml,
      testTimeToml
    ]

testBatteryToml :: TestTree
testBatteryToml = testCase "Reads battery toml" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["battery"]
    expected = BatteryCmd BatteryAppSysFs BatteryFieldPercentage

testGlobalIpToml :: TestTree
testGlobalIpToml = testCase "Reads global-ip toml" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["global-ip"]
    expected =
      GlobalIpCmd
        GlobalIpAppCurl
        (This ["http://whatismyip.akamai.com/", "http://myexternalip.com/raw"])

testMemoryToml :: TestTree
testMemoryToml = testCase "Reads memory toml" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["memory"]
    expected = MemoryCmd MemoryAppFree MemoryFieldUsed MemoryUnitsPercentage

testNetConnToml :: TestTree
testNetConnToml = testCase "Reads net-conn toml" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["net-conn"]
    expected = NetConnCmd NetInterfaceAppIp NetConnFieldIpv4

testNetInterfaceToml :: TestTree
testNetInterfaceToml = testCase "Reads net-if toml" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["net-if"]
    expected =
      NetInterfaceCmd
        NetInterfaceAppNmCli
        "my-wifi-dev"
        NetInterfaceFieldName

testTimeToml :: TestTree
testTimeToml = testCase "Reads time toml" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["time"]
    expected = TimeCmd TimezoneDestUTC "%I:%m %P"

argsTests :: TestTree
argsTests =
  testGroup
    "Args overrides Toml"
    [ testBatteryArgsToml,
      testGlobalIpArgsToml,
      testMemoryArgsToml,
      testNetConnArgsToml,
      testNetInterfaceArgsToml,
      testTimeArgsToml
    ]

testBatteryArgsToml :: TestTree
testBatteryArgsToml = testCase "Uses battery args" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["battery", "-a", "upower", "-f", "status"]
    expected = BatteryCmd BatteryAppUPower BatteryFieldStatus

testGlobalIpArgsToml :: TestTree
testGlobalIpArgsToml = testCase "Uses global-ip args" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args =
      [ "global-ip",
        "-a",
        "dig",
        "-f",
        "both",
        "--ipv4-src",
        "one",
        "--ipv4-src",
        "two",
        "--ipv6-src",
        "three"
      ]
    expected =
      GlobalIpCmd
        GlobalIpAppDig
        ( These
            ["one", "two"]
            ["three"]
        )

testMemoryArgsToml :: TestTree
testMemoryArgsToml = testCase "Uses memory args" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["memory", "-f", "total", "-u", "bytes"]
    expected = MemoryCmd MemoryAppFree MemoryFieldTotal MemoryUnitsBytes

testNetConnArgsToml :: TestTree
testNetConnArgsToml = testCase "Uses net-conn args" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["net-conn", "-a", "nmcli", "-f", "default"]
    expected = NetConnCmd NetInterfaceAppNmCli NetConnFieldDefault

testNetInterfaceArgsToml :: TestTree
testNetInterfaceArgsToml = testCase "Uses net-if args" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["net-if", "-a", "ip", "-f", "default", "-d", "off"]
    expected =
      NetInterfaceCmd
        NetInterfaceAppIp
        NetInterfaceDeviceNone
        NetInterfaceFieldDefault

testTimeArgsToml :: TestTree
testTimeArgsToml = testCase "Uses time args" $ do
  result <- runFinalConfigTomlIO args
  expected @=? result
  where
    args = ["time", "-d", "America/New_York", "-f", "default"]
    expected = TimeCmd (TimezoneDestTZ "America/New_York") TimeFormatDefault

testMissingApps :: TestTree
testMissingApps =
  testGroup
    "Missing apps throws exception"
    [ testMissingBattery,
      testMissingGlobalIp,
      testMissingMemory,
      testMissingNetConn,
      testMissingNetInterface
    ]

testMissingBattery :: TestTree
testMissingBattery = testCase "Battery" $ do
  result <- runFinalConfigExceptionIO args
  "Missing app: battery" @=? displayException result
  where
    args = ["battery"]

testMissingGlobalIp :: TestTree
testMissingGlobalIp = testCase "GlobalIp" $ do
  result <- runFinalConfigExceptionIO args
  "Missing app: global-ip" @=? displayException result
  where
    args = ["global-ip"]

testMissingMemory :: TestTree
testMissingMemory = testCase "Memory" $ do
  result <- runFinalConfigExceptionIO args
  "Missing app: memory" @=? displayException result
  where
    args = ["memory"]

testMissingNetConn :: TestTree
testMissingNetConn = testCase "NetConn" $ do
  result <- runFinalConfigExceptionIO args
  "Missing app: net-conn" @=? displayException result
  where
    args = ["net-conn"]

testMissingNetInterface :: TestTree
testMissingNetInterface = testCase "NetInterface" $ do
  result <- runFinalConfigExceptionIO args
  "Missing app: net-if" @=? displayException result
  where
    args = ["net-if"]

runFinalConfigExceptionIO :: [String] -> IO ConfigException
runFinalConfigExceptionIO args = do
  eResult <-
    try @ConfigException
      $ runConfigEnvIO (withArgs args' Runner.getFinalConfig) ()

  case eResult of
    Left ex -> pure ex
    Right x -> assertFailure $ "Expected ConfigException, received: " <> show x
  where
    args' = ["--config", "off"] <> args

runFinalConfigTomlIO :: [String] -> IO PythiaCommand2
runFinalConfigTomlIO args =
  runConfigEnvIO (withArgs args' Runner.getFinalConfig) ()
  where
    args' = args <> ["-c", "examples/config.toml"]

testMisc :: TestTree
testMisc =
  testGroup
    "Miscellaneous"
    [ testXdg,
      testGlobalIpFieldsOptional
    ]

testXdg :: TestTree
testXdg = testCase "Reads Xdg config" $ do
  result <- runConfigEnvIO (withArgs args Runner.getFinalConfig) xdg
  expected @=? result
  where
    xdg = [osp|test|] </> [osp|integration|]
    args = ["battery"]
    expected = BatteryCmd BatteryAppSysFs BatteryFieldDefault

-- This test is for a bug where source keys were accidentally mandatory.
testGlobalIpFieldsOptional :: TestTree
testGlobalIpFieldsOptional = testCase "Global IP fields optional" $ do
  result <- runConfigEnvIO (withArgs args Runner.getFinalConfig) xdg
  expected @=? result
  where
    xdg = [osp|test|] </> [osp|integration|]
    args = ["global-ip"]
    expected = GlobalIpCmd GlobalIpAppDig (This [])

type ConfigIO = ConfigEnvIO ()

newtype ConfigEnvIO env a = MkConfigIO (ReaderT env IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadOptparse,
      MonadThrow
    )
    via (ReaderT env IO)
  deriving (MonadReader env) via (ReaderT env IO)

deriving newtype instance MonadPathReader ConfigIO

type ConfigXdgIO = ConfigEnvIO OsPath

deriving newtype instance MonadIO ConfigXdgIO

instance MonadPathReader ConfigXdgIO where
  getXdgDirectory XdgConfig p = asks (\d -> d </> [osp|config|] </> p)
  getXdgDirectory _ _ = error "unimplemented"

  doesDirectoryExist = liftIO . doesDirectoryExist

  doesFileExist = liftIO . doesFileExist

runConfigEnvIO :: ConfigEnvIO r a -> r -> IO a
runConfigEnvIO (MkConfigIO rdr) = runReaderT rdr
