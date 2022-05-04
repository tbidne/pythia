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
      queryInterfacesIp,
      queryInterfacesMany
    ]

queryInterfacesNmcli :: TestTree
queryInterfacesNmcli = queryInterfaces (Just "nmcli") "nmcli"

queryInterfacesIp :: TestTree
queryInterfacesIp = queryInterfaces (Just "ip") "ip"

queryInterfacesMany :: TestTree
queryInterfacesMany = queryInterfaces Nothing "many"

queryInterfaces :: Maybe String -> String -> TestTree
queryInterfaces appCmd desc = testCase desc $ do
  let argList = ["net-if"] <> maybe [] (\s -> ["--app", s]) appCmd
  capturePythia argList >>= assertNonEmpty

queryInterfaceTests :: TestTree
queryInterfaceTests =
  testGroup
    "Queries for single interface"
    [ queryInterfaceNmcli,
      queryInterfaceIp,
      queryInterfaceMany
    ]

queryInterfaceNmcli :: TestTree
queryInterfaceNmcli = queryInterface (Just "nmcli") "wlp0s20f3" "nmcli"

queryInterfaceIp :: TestTree
queryInterfaceIp = queryInterface (Just "ip") "wlp0s20f3" "ip"

queryInterfaceMany :: TestTree
queryInterfaceMany = queryInterface Nothing "wlp0s20f3" "many"

queryInterface :: Maybe String -> String -> String -> TestTree
queryInterface appCmd device desc = testCase desc $ do
  let argList =
        ["net-if"]
          <> maybe [] (\s -> ["--app", s]) appCmd
          <> ["--device", device]
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
  let argList = ["net-if", "--field", field]
  capturePythia argList >>= assertNonEmpty
