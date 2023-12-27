module Unit.Pythia.Services.Battery.Types (tests) where

import Pythia.Data.Percentage (unsafePercentage)
import Pythia.Services.Battery.Types
  ( Battery (MkBattery),
    BatteryStatus
      ( Charging,
        Discharging,
        Full,
        Pending
      ),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Battery.Types"
    [ testDisplay
    ]

testDisplay :: TestTree
testDisplay = testCase "Display" $ do
  "Charging: 23%" @=? display (mkBattery 23 Charging)
  "Discharging: 0%" @=? display (mkBattery 0 Discharging)
  "Full: 100%" @=? display (mkBattery 100 Full)
  "Pending: 65%" @=? display (mkBattery 65 Pending)

mkBattery :: Word8 -> BatteryStatus -> Battery
mkBattery p = MkBattery (unsafePercentage p)
