{-# LANGUAGE QuasiQuotes #-}

module Integration.Pythia.Services.Battery (tests) where

import Data.List qualified as L
import Effectful.FileSystem.FileReader.Dynamic (FileReader (ReadBinaryFile))
import Effectful.Process.Typed.Dynamic
  ( ExitCode (ExitSuccess),
    TypedProcess (ReadProcess),
  )
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "battery"
    [ testBatteryDefault,
      testBatteryPercentage,
      testBatteryStatus
    ]

testBatteryDefault :: TestTree
testBatteryDefault = testCase "default" $ do
  acpiResults <- runIntIO ["battery", "-a", "acpi"]
  assertSingleOutput "Charging: 75%" acpiResults
  sysFsResults <- runIntIO ["battery", "-a", "sysfs"]
  assertSingleOutput "Full: 100%" sysFsResults
  upowerResults <- runIntIO ["battery", "-a", "upower"]
  assertSingleOutput "Discharging: 0%" upowerResults

testBatteryPercentage :: TestTree
testBatteryPercentage = testCase "percentage" $ do
  acpiResults <- runIntIO ["battery", "-a", "acpi", "-f", "percentage"]
  assertSingleOutput "75%" acpiResults
  sysFsResults <- runIntIO ["battery", "-a", "sysfs", "-f", "percentage"]
  assertSingleOutput "100%" sysFsResults
  upowerResults <- runIntIO ["battery", "-a", "upower", "-f", "percentage"]
  assertSingleOutput "0%" upowerResults

testBatteryStatus :: TestTree
testBatteryStatus = testCase "status" $ do
  acpiResults <- runIntIO ["battery", "-a", "acpi", "-f", "status"]
  assertSingleOutput "Charging" acpiResults
  sysFsResults <- runIntIO ["battery", "-a", "sysfs", "-f", "status"]
  assertSingleOutput "Full" sysFsResults
  upowerResults <- runIntIO ["battery", "-a", "upower", "-f", "status"]
  assertSingleOutput "Discharging" upowerResults

runIntIO :: [String] -> IO [Text]
runIntIO = runIntegrationIO unIntIO

unIntIO ::
  Eff
    [ Environment,
      FileReader,
      Optparse,
      PathReader,
      Terminal,
      Time,
      TypedProcess,
      Reader (IORef Text),
      IOE
    ]
    a ->
  Eff [Reader (IORef Text), IOE] a
unIntIO =
  runTypedProcessMock
    . runTime
    . runTerminalMock
    . runPathReaderMock
    . runOptparse
    . runFileReaderMock
    . runEnvironment

runFileReaderMock :: Eff (FileReader : es) a -> Eff es a
runFileReaderMock = interpret_ $ \case
  ReadBinaryFile p
    | p == [osp|/sys/class/power_supply/BAT0/capacity|] -> pure "100"
    | p == [osp|/sys/class/power_supply/BAT0/status|] -> pure "full"
    | otherwise -> error $ "Tried to read unexpected file: " <> show p

runTypedProcessMock :: Eff (TypedProcess : es) a -> Eff es a
runTypedProcessMock = interpret_ $ \case
  ReadProcess pc -> case cmd of
    "Shell command: acpi" -> pure (ExitSuccess, "Battery 0: Charging, 75%", "")
    "Shell command: upower -i `upower -e | grep 'BAT'`" ->
      let output =
            L.unlines
              [ "native-path:          BAT0",
                "vendor:               LGC",
                "model:                5B10W13930",
                "serial:               6299",
                "power supply:         yes",
                "updated:              Thu 04 Jan 2024 08:05:53 AM NZDT (18 seconds ago)",
                "has history:          yes",
                "has statistics:       yes",
                "battery",
                "  present:             yes",
                "  rechargeable:        yes",
                "  state:               discharging",
                "  warning-level:       none",
                "  energy:              46.01 Wh",
                "  energy-empty:        0 Wh",
                "  energy-full:         51.99 Wh",
                "  energy-full-design:  51 Wh",
                "  energy-rate:         11.716 W",
                "  voltage:             17.129 V",
                "  charge-cycles:       1380",
                "  time to full:        30.6 minutes",
                "  percentage:          0%",
                "  capacity:            100%",
                "  technology:          lithium-polymer",
                "  icon-name:          'battery-full-charging-symbolic'",
                "History (charge):",
                "  1704308723\t88.000\tcharging",
                "History (rate):",
                "  1704308753\t11.716\tcharging",
                "  1704308723\t11.954\tcharging",
                "  1704308693\t12.210\tcharging",
                "  1704308663\t12.465\tcharging"
              ]
       in pure (ExitSuccess, fromString output, "")
    bad -> error $ "Unexpected command: " <> bad
    where
      cmd = processConfigToCmd pc
  _ -> error "runTypedProcessMock: unimplemented"
