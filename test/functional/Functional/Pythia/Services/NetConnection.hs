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
    [ runApps (Just "nmcli") "nmcli",
      runApps (Just "ip") "ip",
      runApps Nothing "<default>"
    ]

runApps :: Maybe String -> String -> TestTree
runApps appCmd desc = testCase ("Finds live interface with " <> desc) $ do
  let argList =
        ["net-conn"]
          <> (maybe [] (\s -> ["--app", s]) appCmd)
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
  let argList = ["net-conn", "--field", field]
  capturePythia argList >>= assertNonEmpty
