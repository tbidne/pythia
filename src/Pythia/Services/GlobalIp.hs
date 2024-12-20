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
    MonadCatch m,
    MonadTypedProcess m
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv4] ->
  [UrlSource Ipv6] ->
  m (IpAddress Ipv4, IpAddress Ipv6)
queryGlobalIp app ipv4Srcs ipv6Srcs = getBothIps app (ipv4Srcs, ipv6Srcs)
{-# INLINEABLE queryGlobalIp #-}

-- | 'queryGlobalIp' restricted to IPv4 address only.
--
-- @since 0.1
queryGlobalIpv4 ::
  ( HasCallStack,
    MonadCatch m,
    MonadTypedProcess m
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv4] ->
  m (IpAddress Ipv4)
queryGlobalIpv4 = getIpv4s
{-# INLINEABLE queryGlobalIpv4 #-}

-- | 'queryGlobalIp' restricted to IPv6 address only.
--
-- @since 0.1
queryGlobalIpv6 ::
  ( HasCallStack,
    MonadCatch m,
    MonadTypedProcess m
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv6] ->
  m (IpAddress Ipv6)
queryGlobalIpv6 = getIpv6s
{-# INLINEABLE queryGlobalIpv6 #-}

getBothIps ::
  ( HasCallStack,
    MonadCatch m,
    MonadTypedProcess m
  ) =>
  GlobalIpApp ->
  ([UrlSource Ipv4], [UrlSource Ipv6]) ->
  m (IpAddress Ipv4, IpAddress Ipv6)
getBothIps app (ipv4Srcs, ipv6Srcs) =
  (,)
    <$> getIpv4s app ipv4Srcs
    <*> getIpv6s app ipv6Srcs
{-# INLINEABLE getBothIps #-}

getIpv4s ::
  ( HasCallStack,
    MonadCatch m,
    MonadTypedProcess m
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv4] ->
  m (IpAddress Ipv4)
getIpv4s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv4Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources
{-# INLINEABLE getIpv4s #-}

getIpv6s ::
  ( HasCallStack,
    MonadCatch m,
    MonadTypedProcess m
  ) =>
  GlobalIpApp ->
  [UrlSource Ipv6] ->
  m (IpAddress Ipv6)
getIpv6s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv6Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources
{-# INLINEABLE getIpv6s #-}

prependApp :: GlobalIpApp -> [UrlSource a] -> [UrlSource a]
prependApp GlobalIpAppCurl srcs = fmap (#unUrlSource %~ ("curl " <>)) srcs
prependApp GlobalIpAppDig srcs = fmap (#unUrlSource %~ (\s -> "dig " <> s <> " +short")) srcs
{-# INLINEABLE prependApp #-}

ipv4Defaults :: GlobalIpApp -> [UrlSource Ipv4]
ipv4Defaults GlobalIpAppCurl = curlDefaults ^. _1
ipv4Defaults GlobalIpAppDig = digDefaults ^. _1
{-# INLINEABLE ipv4Defaults #-}

ipv6Defaults :: GlobalIpApp -> [UrlSource Ipv6]
ipv6Defaults GlobalIpAppCurl = curlDefaults ^. _2
ipv6Defaults GlobalIpAppDig = digDefaults ^. _2
{-# INLINEABLE ipv6Defaults #-}

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
{-# INLINEABLE curlDefaults #-}

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
{-# INLINEABLE digDefaults #-}

getIpFromSources ::
  ( HasCallStack,
    MonadCatch m,
    MonadTypedProcess m,
    Predicate (IpRefinement a) Text
  ) =>
  [UrlSource a] ->
  m (IpAddress a)
getIpFromSources = fmap MkIpAddress . getIp (#unUrlSource % re #unCommand)
{-# INLINEABLE getIpFromSources #-}

getIp ::
  forall m p a.
  ( HasCallStack,
    MonadCatch m,
    MonadTypedProcess m,
    Predicate p Text
  ) =>
  Iso' a Command ->
  [a] ->
  m (Refined p Text)
getIp cmdIso cmds = ShellApp.tryIOs (fmap go cmds)
  where
    go cmd = do
      txt <- ShellApp.runCommand $ cmd ^. cmdIso
      R.refineThrow (trim txt)
{-# INLINEABLE getIp #-}

trim :: Text -> Text
trim = T.dropAround Char.isSpace
{-# INLINEABLE trim #-}
