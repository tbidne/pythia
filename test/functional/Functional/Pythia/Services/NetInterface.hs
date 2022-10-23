-- | NetInterface tests.
--
-- @since 0.1
module Functional.Pythia.Services.NetInterface
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "net-if"
    [ queryInterfacesTests,
      queryInterfaceTests,
      testFields
    ]

queryInterfacesTests :: TestTree
queryInterfacesTests =
  testGroup
    "Queries for all interfaces"
    [ queryInterfacesNmcli,
      queryInterfacesIp
    ]

queryInterfacesNmcli :: TestTree
queryInterfacesNmcli = queryInterfaces "nmcli" "nmcli"

queryInterfacesIp :: TestTree
queryInterfacesIp = queryInterfaces "ip" "ip"

queryInterfaces :: String -> String -> TestTree
queryInterfaces appCmd desc = testCase desc $ do
  let argList = ["net-if", "--app", appCmd]
  capturePythia argList >>= assertNonEmpty

queryInterfaceTests :: TestTree
queryInterfaceTests =
  testGroup
    "Queries for single interface"
    [ queryInterfaceNmcli,
      queryInterfaceIp
    ]

queryInterfaceNmcli :: TestTree
queryInterfaceNmcli = queryInterface "nmcli" "wlp0s20f3" "nmcli"

queryInterfaceIp :: TestTree
queryInterfaceIp = queryInterface "ip" "wlp0s20f3" "ip"

queryInterface :: String -> String -> String -> TestTree
queryInterface appCmd device desc = testCase desc $ do
  let argList = ["net-if", "--app", appCmd, "--device", device]
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
  let argList = ["net-if", "--field", field, "--app", "nmcli"]
  capturePythia argList >>= assertNonEmpty
