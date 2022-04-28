-- | Memory tests.
--
-- @since 0.1
module Functional.Pythia.Services.Memory
  ( tests,
  )
where

import Functional.Prelude
-- import Pythia.Services.Memory (MemoryApp (..), MemoryConfig (..), RunApp (..))
-- import Pythia.Services.Memory qualified as Memory
import Pythia.Services.Memory (MemoryApp (..), MemoryConfig (..), RunApp (..))
import Pythia.Services.Memory qualified as Memory

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Memory"
    [ runsMany,
      runsFree
    ]

runsFree :: TestTree
runsFree = runsSingle MemoryFree "Free"

runsSingle :: MemoryApp -> String -> TestTree
runsSingle app appName = testCase ("Runs " <> appName) $ do
  let config = MkMemoryConfig $ Single app
  result <- Memory.queryMemory config
  result @=? result

runsMany :: TestTree
runsMany = testCase "Runs Many" $ do
  let config = MkMemoryConfig Many
  result <- Memory.queryMemory config
  result @=? result
