module Unit.Pythia.Services.Battery.Acpi
  ( tests,
  )
where

import Data.Text qualified as T
import Pythia.Data.Percentage (_MkPercentage)
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.Types (BatteryStatus (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Battery.Acpi"
    [ parseCharging,
      parseDischarging,
      parseFull,
      parsePending,
      unknownStatusFails,
      parseFirst,
      parseSecond,
      parseMiddle
    ]

parseCharging :: TestTree
parseCharging = parseX 20 ("Charging", Charging)

parseDischarging :: TestTree
parseDischarging = parseX 80 ("Discharging", Discharging)

parseFull :: TestTree
parseFull = parseX 100 ("Full", Full)

parsePending :: TestTree
parsePending = parseX 50 ("Not charging", Pending)

unknownStatusFails :: TestTree
unknownStatusFails = testCase "Unknown status fails" $ do
  let result = Acpi.parseBattery (battery 80 "bad status")
  assertBool "Acpi unknown status should be Left" (isLeft result)

parseFirst :: TestTree
parseFirst = testCase "Parses second battery" $ do
  let result = Acpi.parseBattery batteryTxt
  Just Discharging @=? result ^? _Right % #status
  Just 73 @=? result ^? _Right % #percentage % _MkPercentage
  where
    batteryTxt =
      T.unlines
        [ "Battery 1: Discharging, 73%, 02:38:37 remaining",
          "Battery 0: Unknown, 0%, rate information unavailable"
        ]

parseSecond :: TestTree
parseSecond = testCase "Parses second battery" $ do
  let result = Acpi.parseBattery batteryTxt
  Just Discharging @=? result ^? _Right % #status
  Just 73 @=? result ^? _Right % #percentage % _MkPercentage
  where
    batteryTxt =
      T.unlines
        [ "Battery 0: Unknown, 0%, rate information unavailable",
          "Battery 1: Discharging, 73%, 02:38:37 remaining"
        ]

parseMiddle :: TestTree
parseMiddle = testCase "Parses second battery" $ do
  let result = Acpi.parseBattery batteryTxt
  Just Discharging @=? result ^? _Right % #status
  Just 73 @=? result ^? _Right % #percentage % _MkPercentage
  where
    batteryTxt =
      T.unlines
        [ "Battery 0: Unknown, 0%, rate information unavailable",
          "Battery 1: Discharging, 73%, 02:38:37 remaining",
          "Battery 0: Unknown, 0%, rate information unavailable"
        ]

parseX :: Word8 -> (Text, BatteryStatus) -> TestTree
parseX lvl (csTxt, cs) = testCase desc $ do
  let result = Acpi.parseBattery (battery lvl csTxt)
  Just cs @=? result ^? _Right % #status
  Just lvl @=? result ^? _Right % #percentage % _MkPercentage
  where
    desc = "Parses percentage " <> show lvl <> ", state " <> T.unpack csTxt

battery :: Word8 -> Text -> Text
battery percentage state =
  "Battery 0: "
    <> state
    <> ", "
    <> T.pack (show percentage)
    <> "%, 00:10:43 until charged"
