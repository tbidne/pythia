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
    [ findUpInterface (Just "nmcli") "nmcli",
      findUpInterface (Just "ip") "ip",
      findUpInterface Nothing "<unspecified>"
    ]

findUpInterface :: Maybe String -> String -> TestTree
findUpInterface appCmd desc = testCase ("Finds live interface with " <> desc) $ do
  let argList =
        ["net-conn"]
          <> (maybe [] (\s -> ["--app", s]) appCmd)
  capturePythia argList >>= assertNonEmpty
