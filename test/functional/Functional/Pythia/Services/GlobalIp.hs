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
    "Pythia.Services.GlobalIp"
    [ testApps,
      testCustomSources
    ]

testApps :: TestTree
testApps =
  testGroup
    "Tests apps with defaults"
    [ runsCurlDefault,
      runsDigDefault,
      runsManyDefault
    ]

runsCurlDefault :: TestTree
runsCurlDefault = runsApp (Just "curl") [] "curl"

runsDigDefault :: TestTree
runsDigDefault = runsApp (Just "dig") [] "dig"

runsManyDefault :: TestTree
runsManyDefault = runsApp Nothing [] "many"

testCustomSources :: TestTree
testCustomSources =
  testGroup
    "Uses custom sources"
    [ runsCurlIpv4CustomSrcs,
      runsDigIpv4CustomSrcs,
      runsManyIpv4CustomSrcs
    ]

runsCurlIpv4CustomSrcs :: TestTree
runsCurlIpv4CustomSrcs = runsApp (Just "curl") srcs "curl"
  where
    srcs = ["--ipv4-src", "http://whatismyip.akamai.com/"]

runsDigIpv4CustomSrcs :: TestTree
runsDigIpv4CustomSrcs = runsApp (Just "dig") srcs "dig"
  where
    srcs = ["--ipv4-src", "@resolver1.opendns.com myip.opendns.com"]

runsManyIpv4CustomSrcs :: TestTree
runsManyIpv4CustomSrcs = runsApp Nothing srcs "many"
  where
    srcs =
      [ "--ipv4-src",
        "http://whatismyip.akamai.com/",
        "--ipv4-src",
        "@resolver1.opendns.com myip.opendns.com"
      ]

runsApp :: Maybe String -> [String] -> String -> TestTree
runsApp appCmd customSrcs desc = testCase desc $ do
  let argList =
        ["global-ip"]
          <> (maybe [] (\s -> ["--app", s]) appCmd)
          <> customSrcs
  capturePythia argList >>= assertNonEmpty
