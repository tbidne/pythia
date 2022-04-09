-- | GlobalIp tests.
--
-- @since 0.1
module Functional.Pythia.Services.GlobalIp
  ( tests,
  )
where

import Functional.Prelude
import Pythia.Services.GlobalIp
  ( GlobalIpApp (..),
    GlobalIpConfig (..),
    IpType (..),
    RunApp (..),
    UrlSource (..),
  )
import Pythia.Services.GlobalIp qualified as GlobalIp

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Pythia.Services.GlobalIp"
    [ defaultSources,
      customSources
    ]

defaultSources :: TestTree
defaultSources =
  testGroup
    "Uses default sources"
    [ runsCurlIpv4DefSrcs,
      runsDigIpv4DefSrcs,
      runsManyIpv4DefSrcs
    ]

runsCurlIpv4DefSrcs :: TestTree
runsCurlIpv4DefSrcs = runsIpv4 (Single GlobalIpCurl) [] "Retrieves global ip via curl"

runsDigIpv4DefSrcs :: TestTree
runsDigIpv4DefSrcs = runsIpv4 (Single GlobalIpDig) [] "Retrieves global ip via dig"

runsManyIpv4DefSrcs :: TestTree
runsManyIpv4DefSrcs = runsIpv4 Many [] "Retrieves global ip many"

customSources :: TestTree
customSources =
  testGroup
    "Uses custom sources"
    [ runsCurlIpv4CustomSrcs,
      runsDigIpv4CustomSrcs,
      runsManyIpv4CustomSrcs
    ]

runsCurlIpv4CustomSrcs :: TestTree
runsCurlIpv4CustomSrcs = runsIpv4 (Single GlobalIpCurl) srcs "Retrieves global ip via curl"
  where
    srcs = ["http://whatismyip.akamai.com/"]

runsDigIpv4CustomSrcs :: TestTree
runsDigIpv4CustomSrcs = runsIpv4 (Single GlobalIpDig) srcs "Retrieves global ip via dig"
  where
    srcs = ["@resolver1.opendns.com myip.opendns.com"]

runsManyIpv4CustomSrcs :: TestTree
runsManyIpv4CustomSrcs = runsIpv4 Many srcs "Retrieves global ip many"
  where
    srcs =
      [ "http://whatismyip.akamai.com/",
        "@resolver1.opendns.com myip.opendns.com"
      ]

runsIpv4 :: RunApp GlobalIpApp -> [UrlSource 'Ipv4] -> String -> TestTree
runsIpv4 app sources desc = testCase desc $ do
  let config = MkGlobalIpConfig app sources
  result <- GlobalIp.queryGlobalIpv4 config
  result @=? result
