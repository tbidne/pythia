module Unit.Pythia.Services.Battery.UPower
  ( tests,
  )
where

import Data.Text qualified as T
import Pythia.Data.Percentage (_MkPercentage)
import Pythia.Services.Battery.Types (BatteryStatus (..))
import Pythia.Services.Battery.UPower qualified as UPower
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Battery.UPower"
    [ parseCharging,
      parseDischarging,
      parseFull,
      parsePending,
      unknownStatusFails
    ]

parseCharging :: TestTree
parseCharging = parseX 20 ("charging", Charging)

parseDischarging :: TestTree
parseDischarging = parseX 80 ("discharging", Discharging)

parseFull :: TestTree
parseFull = parseX 100 ("fully-charged", Full)

parsePending :: TestTree
parsePending = parseX 100 ("pending-charge", Pending)

unknownStatusFails :: TestTree
unknownStatusFails = testCase "Unknown status fails" $ do
  let result = UPower.parseBattery (state 80 "bad status")
  assertBool "UPower unknown status should be Left" (isLeft result)

parseX :: Word8 -> (Text, BatteryStatus) -> TestTree
parseX lvl (csTxt, cs) = testCase desc $ do
  let result = UPower.parseBattery (state lvl csTxt)
  Just cs @=? result ^? _Right % #status
  Just lvl @=? result ^? _Right % #percentage % _MkPercentage
  where
    desc = "Parses percentage " <> show lvl <> ", status " <> T.unpack csTxt

state :: Word8 -> Text -> Text
state percentage status =
  T.unlines
    [ "native-path:          BAT0",
      "vendor:               LGC",
      "model:                5B10W13930",
      "serial:               6299",
      "power supply:         yes",
      "updated:              Sun 27 Mar 2022 08:16:55 PM NZDT (22 seconds ago)",
      "has history:          yes",
      "has statistics:       yes",
      "battery",
      "  present:             yes",
      "  rechargeable:        yes",
      "  state:               " <> status,
      "  warning-percentage:       none",
      "  energy:              47.73 Wh",
      "  energy-empty:        0 Wh",
      "  energy-full:         51.99 Wh",
      "  energy-full-design:  51 Wh",
      "  energy-rate:         12.278 W",
      "  voltage:             17.125 V",
      "  charge-cycles:       739",
      "  time to full:        20.8 minutes",
      "  percentage:          " <> T.pack (show percentage) <> "%",
      "  capacity:            100%",
      "  technology:          lithium-polymer",
      "  icon-name:          'battery-full-charging-symbolic'",
      "History (charge):",
      "  1648365415 91.000 charging",
      "History (rate):",
      "  1648365415 12.278 charging"
    ]
