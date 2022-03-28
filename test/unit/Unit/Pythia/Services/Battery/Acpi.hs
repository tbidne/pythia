module Unit.Pythia.Services.Battery.Acpi
  ( tests,
  )
where

import Data.Text qualified as T
import Numeric.Data.Interval (LRInterval (..))
import Pythia.Services.Battery.Acpi qualified as Acpi
import Pythia.Services.Battery.Types (BatteryState (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Battery.Acpi"
    [ parseCharging,
      parseDischarging,
      parseFull,
      parsePending,
      parseUnknown
    ]

parseCharging :: TestTree
parseCharging = parseX 20 ("Charging", Charging)

parseDischarging :: TestTree
parseDischarging = parseX 80 ("Discharging", Discharging)

parseFull :: TestTree
parseFull = parseX 100 ("Full", Full)

parsePending :: TestTree
parsePending = parseX 50 ("Not charging", Pending)

parseUnknown :: TestTree
parseUnknown = parseX 20 ("some bad status-20", Unknown "some bad status-20")

parseX :: Int -> (Text, BatteryState) -> TestTree
parseX lvl (csTxt, cs) = testCase desc $ do
  parser <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (Acpi.batteryShellApp ^? #_SimpleApp % #parser)
  let result = parser (battery lvl csTxt)
  Just cs @=? result ^? #_Right % #status
  Just (MkLRInterval lvl) @=? result ^? #_Right % #level
  where
    desc = "Parses percentage " <> show lvl <> ", state " <> T.unpack csTxt

battery :: Int -> Text -> Text
battery percentage state =
  "Battery 0: "
    <> state
    <> ", "
    <> T.pack (show percentage)
    <> "%, 00:10:43 until charged"
