-- | NetInterface tests.
--
-- @since 0.1
module Functional.Pythia.Services.NetConnection
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "net-conn"
    [ testApps,
      testFields
    ]

testApps :: TestTree
testApps =
  testGroup
    "Test Apps"
    [ runApps "nmcli" "nmcli",
      runApps "ip" "ip"
    ]

runApps :: String -> String -> TestTree
runApps appCmd desc = testCase ("Finds live interface with " <> desc) $ do
  let argList = ["net-conn", "--app", appCmd]
  capturePythia argList >>= assertNonEmpty

testFields :: TestTree
testFields =
  testGroup
    "Test Fields"
    [ runsField "name",
      runsField "ipv4",
      runsField "ipv6"
    ]

runsField :: String -> TestTree
runsField field = testCase field $ do
  let argList = ["net-conn", "--field", field, "--app", "nmcli"]
  capturePythia argList >>= assertNonEmpty
