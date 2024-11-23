-- | This modules exports everything needed for retrieving global
-- IP addresses.
--
-- @since 0.1
module Pythia.Services.GlobalIp
  ( -- * Queries
    queryGlobalIp,
    queryGlobalIpv4,
    queryGlobalIpv6,

    -- * Types
    IpType (..),
    IpAddress (..),

    -- ** Configuration
    GlobalIpApp (..),
    UrlSource (..),
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Pythia.Data.Command (Command)
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.GlobalIp.Types
  ( GlobalIpApp (GlobalIpAppCurl, GlobalIpAppDig),
    UrlSource (MkUrlSource, unUrlSource),
  )
import Pythia.Services.Types.Network
  ( IpAddress (MkIpAddress),
    IpRefinement,
    IpType (Ipv4, Ipv6),
  )
import Refined (Predicate, Refined)
import Refined qualified as R

-- | Queries for IPv4 and IPv6 global IP address based on the configuration.
--
-- @since 0.1
queryGlobalIp ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv4] ->
  [UrlSource Ipv6] ->
  Eff es (IpAddress Ipv4, IpAddress Ipv6)
queryGlobalIp app ipv4Srcs ipv6Srcs = getBothIps app (ipv4Srcs, ipv6Srcs)

-- | 'queryGlobalIp' restricted to IPv4 address only.
--
-- @since 0.1
queryGlobalIpv4 ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv4] ->
  Eff es (IpAddress Ipv4)
queryGlobalIpv4 = getIpv4s

-- | 'queryGlobalIp' restricted to IPv6 address only.
--
-- @since 0.1
queryGlobalIpv6 ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv6] ->
  Eff es (IpAddress Ipv6)
queryGlobalIpv6 = getIpv6s

getBothIps ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  GlobalIpApp ->
  ([UrlSource Ipv4], [UrlSource Ipv6]) ->
  Eff es (IpAddress Ipv4, IpAddress Ipv6)
getBothIps app (ipv4Srcs, ipv6Srcs) =
  (,)
    <$> getIpv4s app ipv4Srcs
    <*> getIpv6s app ipv6Srcs

getIpv4s ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv4] ->
  Eff es (IpAddress Ipv4)
getIpv4s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv4Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources

getIpv6s ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv6] ->
  Eff es (IpAddress Ipv6)
getIpv6s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv6Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources

prependApp :: GlobalIpApp -> [UrlSource a] -> [UrlSource a]
prependApp GlobalIpAppCurl srcs = fmap (#unUrlSource %~ ("curl " <>)) srcs
prependApp GlobalIpAppDig srcs = fmap (#unUrlSource %~ (\s -> "dig " <> s <> " +short")) srcs

ipv4Defaults :: GlobalIpApp -> [UrlSource Ipv4]
ipv4Defaults GlobalIpAppCurl = curlDefaults ^. _1
ipv4Defaults GlobalIpAppDig = digDefaults ^. _1

ipv6Defaults :: GlobalIpApp -> [UrlSource Ipv6]
ipv6Defaults GlobalIpAppCurl = curlDefaults ^. _2
ipv6Defaults GlobalIpAppDig = digDefaults ^. _2

curlDefaults :: ([UrlSource Ipv4], [UrlSource Ipv6])
curlDefaults = (ipv4s, ipv6s)
  where
    ipv4s =
      [ "curl http://whatismyip.akamai.com/",
        "curl http://ifconfig.me/ip",
        "curl http://myexternalip.com/raw",
        "curl http://checkip.amazonaws.com/"
      ]
    ipv6s = []

digDefaults :: ([UrlSource Ipv4], [UrlSource Ipv6])
digDefaults = (ipv4s, ipv6s)
  where
    ipv4s =
      [ "dig @resolver1.opendns.com myip.opendns.com +short",
        "dig @resolver2.opendns.com myip.opendns.com +short",
        "dig @resolver3.opendns.com myip.opendns.com +short",
        "dig @resolver4.opendns.com myip.opendns.com +short",
        "dig @ns1-1.akamaitech.net ANY whoami.akamai.net +short",
        "dig -4 TXT o-o.myaddr.l.google.com @ns1.google.com +short"
      ]
    ipv6s = []

getIpFromSources ::
  ( HasCallStack,
    TypedProcess :> es,
    Predicate (IpRefinement a) Text
  ) =>
  [UrlSource a] ->
  Eff es (IpAddress a)
getIpFromSources = fmap MkIpAddress . getIp (#unUrlSource % re #unCommand)

getIp ::
  forall p es a.
  ( HasCallStack,
    TypedProcess :> es,
    Predicate p Text
  ) =>
  Iso' a Command ->
  [a] ->
  Eff es (Refined p Text)
getIp cmdIso cmds = ShellApp.tryIOs (fmap go cmds)
  where
    go cmd = do
      txt <- ShellApp.runCommand $ cmd ^. cmdIso
      R.refineThrow (trim txt)

trim :: Text -> Text
trim = T.dropAround Char.isSpace
