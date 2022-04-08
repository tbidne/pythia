-- | NetInterface tests.
--
-- @since 0.1
module Functional.Pythia.Services.NetInterface
  ( tests,
  )
where

import Data.Maybe (isJust)
import Functional.Prelude
import Pythia.Services.NetInterface
  ( Device (..),
    NetInterfaceApp (..),
    NetInterfaceConfig (..),
    RunApp (..),
  )
import Pythia.Services.NetInterface qualified as NetInterface

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Pythia.Services.NetInterface"
    [ queryInterfacesTests,
      queryInterfaceTests,
      findUpInterface
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
queryInterfacesNmcli = queryInterfaces (Single NetInterfaceNmCli) desc
  where
    desc = "Retrieves net interfaces via nmcli"

queryInterfacesIp :: TestTree
queryInterfacesIp = queryInterfaces (Single NetInterfaceIp) desc
  where
    desc = "Retrieves net interfaces via curl"

queryInterfacesMany :: TestTree
queryInterfacesMany = queryInterfaces Many desc
  where
    desc = "Retrieves net interfaces via many"

queryInterfaces :: RunApp NetInterfaceApp -> String -> TestTree
queryInterfaces app desc = testCase desc $ do
  let config = MkNetInterfaceConfig app
  result <- NetInterface.queryNetInterfaces config
  assertBool "Expected results to be non-empty" (len result > 0)
  where
    len = length . view #unNetInterfaces

queryInterfaceTests :: TestTree
queryInterfaceTests =
  testGroup
    "Queries for single interface"
    [ queryInterfaceNmcli,
      queryInterfaceIp,
      queryInterfaceMany
    ]

queryInterfaceNmcli :: TestTree
queryInterfaceNmcli = queryInterface (Single NetInterfaceNmCli) "wlp0s20f3" desc
  where
    desc = "Retrieves net interface via nmcli"

queryInterfaceIp :: TestTree
queryInterfaceIp = queryInterface (Single NetInterfaceIp) "wlp0s20f3" desc
  where
    desc = "Retrieves net interface via curl"

queryInterfaceMany :: TestTree
queryInterfaceMany = queryInterface Many "wlp0s20f3" desc
  where
    desc = "Retrieves net interface via many"

queryInterface :: RunApp NetInterfaceApp -> Device -> String -> TestTree
queryInterface app device desc = testCase desc $ do
  let config = MkNetInterfaceConfig app
  result <- NetInterface.queryNetInterface device config
  result @=? result

findUpInterface :: TestTree
findUpInterface = testCase "Finds live interface" $ do
  result <- NetInterface.queryNetInterfaces mempty
  result @=? result
  assertBool "Should have found live interface" (isJust (NetInterface.findUp result))
