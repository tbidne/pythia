module Unit.Pythia.Services.Battery.Acpi
  ( tests,
  )
where

import Data.Text qualified as T
import Numeric.Data.Interval (LRInterval (..))
import Pythia.Data.Percentage (percentageIso)
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
      unknownStatusFails
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

parseX :: Word8 -> (Text, BatteryStatus) -> TestTree
parseX lvl (csTxt, cs) = testCase desc $ do
  let result = Acpi.parseBattery (battery lvl csTxt)
  Just cs @=? result ^? _Right % #status
  Just (MkLRInterval lvl) @=? result ^? _Right % #percentage % percentageIso
  where
    desc = "Parses percentage " <> show lvl <> ", state " <> T.unpack csTxt

battery :: Word8 -> Text -> Text
battery percentage state =
  "Battery 0: "
    <> state
    <> ", "
    <> T.pack (show percentage)
    <> "%, 00:10:43 until charged"
