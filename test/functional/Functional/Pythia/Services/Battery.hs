-- | Battery tests.
--
-- @since 0.1
module Functional.Pythia.Services.Battery
  ( tests,
  )
where

import Data.Text qualified as T
import Functional.Prelude
import Text.Read qualified as TR

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "battery"
    [ testApps,
      testStatus,
      testPercentage
    ]

testApps :: TestTree
testApps =
  testGroup
    "Tests Apps"
    [ runsAcpi,
      runsSysFs,
      runsUPower
    ]

runsAcpi :: TestTree
runsAcpi = runsApp "acpi" "acpi"

runsSysFs :: TestTree
runsSysFs = runsApp "sysfs" "sysfs"

runsUPower :: TestTree
runsUPower = runsApp "upower" "upower"

runsApp :: String -> String -> TestTree
runsApp appCmd appName = testCase ("Runs " <> appName) $ do
  let argList = ["battery", "--app", appCmd]
  capturePythia argList >>= assertNonEmpty

testStatus :: TestTree
testStatus = testCase "Tests status" $ do
  let argList = ["battery", "--field", "status", "--app", "acpi"]
  result <- capturePythia argList
  assertBool ("Verify status: " <> T.unpack result) (verifyStatus result)
  where
    verifyStatus s =
      s == "Charging"
        || s == "Discharging"
        || s == "Full"
        || s == "Pending"

testPercentage :: TestTree
testPercentage = testCase "Tests percentage" $ do
  let argList = ["battery", "--field", "percentage", "--app", "sysfs"]
  result <- capturePythia argList
  result' <- case T.unpack result of
    [d1, d2, '%'] -> pure [d1, d2]
    "100%" -> pure "100"
    _ -> assertFailure $ T.unpack $ "Bad percentage: " <> result
  case TR.readMaybe @Int result' of
    Nothing -> assertFailure $ "Could not read percentage: " <> result'
    Just n -> assertBool ("Percentage not in [0, 100]: " <> show n) (0 <= n && n <= 100)
