-- | Battery tests.
--
-- @since 0.1
module Functional.Pythia.Services.Battery
  ( tests,
  )
where

import Functional.Prelude
import Pythia.Services.Battery (BatteryApp (..), BatteryConfig (..), RunApp (..))
import Pythia.Services.Battery qualified as Battery

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Battery"
    [ runsMany,
      runsAcpi,
      runsSysFs,
      runsUPower
    ]

runsAcpi :: TestTree
runsAcpi = runsSingle BatteryAcpi "Acpi"

runsSysFs :: TestTree
runsSysFs = runsSingle BatterySysFs "SysFs"

runsUPower :: TestTree
runsUPower = runsSingle BatteryUPower "UPower"

runsSingle :: BatteryApp -> String -> TestTree
runsSingle app appName = testCase ("Runs " <> appName) $ do
  let config = MkBatteryConfig $ Single app
  result <- Battery.queryBattery config
  result @=? result

runsMany :: TestTree
runsMany = testCase "Runs Many" $ do
  let config = MkBatteryConfig Many
  result <- Battery.queryBattery config
  result @=? result
