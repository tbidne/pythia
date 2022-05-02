-- | Memory tests.
--
-- @since 0.1
module Functional.Pythia.Services.Memory
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Memory"
    [ runsMany,
      runsFree
    ]

runsFree :: TestTree
runsFree = runsApp (Just "free") "free"

runsMany :: TestTree
runsMany = runsApp Nothing "many"

runsApp :: Maybe String -> String -> TestTree
runsApp appCmd desc = testCase desc $ do
  let argList =
        ["memory"]
          <> (maybe [] (\s -> ["--app", s]) appCmd)
  capturePythia argList >>= assertNonEmpty
