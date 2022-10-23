-- | GlobalIp tests.
--
-- @since 0.1
module Functional.Pythia.Services.GlobalIp
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "global-ip"
    [ testApps,
      testCustomSources
    ]

testApps :: TestTree
testApps =
  testGroup
    "Tests apps with defaults"
    [ runsCurlDefault,
      runsDigDefault
    ]

runsCurlDefault :: TestTree
runsCurlDefault = runsApp "curl" [] "curl"

runsDigDefault :: TestTree
runsDigDefault = runsApp "dig" [] "dig"

testCustomSources :: TestTree
testCustomSources =
  testGroup
    "Uses custom sources"
    [ runsCurlIpv4CustomSrcs,
      runsDigIpv4CustomSrcs
    ]

runsCurlIpv4CustomSrcs :: TestTree
runsCurlIpv4CustomSrcs = runsApp "curl" srcs "curl"
  where
    srcs = ["--ipv4-src", "http://whatismyip.akamai.com/"]

runsDigIpv4CustomSrcs :: TestTree
runsDigIpv4CustomSrcs = runsApp "dig" srcs "dig"
  where
    srcs = ["--ipv4-src", "@resolver1.opendns.com myip.opendns.com"]

runsApp :: String -> [String] -> String -> TestTree
runsApp appCmd customSrcs desc = testCase desc $ do
  let argList =
        ["global-ip", "--app", appCmd]
          <> customSrcs
  capturePythia argList >>= assertNonEmpty
