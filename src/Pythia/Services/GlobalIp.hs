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
    GlobalIpv4Config,
    GlobalIpv6Config,
    GlobalIpBothConfig,
    GlobalIpConfig (..),
    GlobalIpApp (..),
    UrlSource (..),
    RunApp (..),
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Optics.Core (Iso', Lens')
import Pythia.Data.Command (Command)
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Prelude
import Pythia.Services.GlobalIp.Types
  ( GlobalIpApp (..),
    GlobalIpBothConfig,
    GlobalIpConfig (..),
    GlobalIpv4Config,
    GlobalIpv6Config,
    UrlSource (..),
  )
import Pythia.Services.GlobalIp.Types qualified as GIpTypes
import Pythia.Services.Types.Network
  ( IpAddress (..),
    IpRefinement,
    IpType (..),
  )
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R

-- | Queries for IPv4 and IPv6 global IP address based on the configuration.
-- If 'app' is 'Many' then we try supported apps in the following
-- order:
--
-- @
-- ['GlobalIpDig', 'GlobalIpCurl']
-- @
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryGlobalIp :: GlobalIpBothConfig -> IO (IpAddress 'Ipv4, IpAddress 'Ipv6)
queryGlobalIp = queryGlobalIp' #app #sources getBothIps
{-# INLINEABLE queryGlobalIp #-}

-- | 'queryGlobalIp' restricted to IPv4 address only.
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryGlobalIpv4 :: GlobalIpv4Config -> IO (IpAddress 'Ipv4)
queryGlobalIpv4 = queryGlobalIp' #app #sources getIpv4s
{-# INLINEABLE queryGlobalIpv4 #-}

-- | 'queryGlobalIp' restricted to IPv6 address only.
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryGlobalIpv6 :: GlobalIpv6Config -> IO (IpAddress 'Ipv6)
queryGlobalIpv6 = queryGlobalIp' #app #sources getIpv6s
{-# INLINEABLE queryGlobalIpv6 #-}

queryGlobalIp' ::
  Lens' config (RunApp GlobalIpApp) ->
  Lens' config sources ->
  (GlobalIpApp -> sources -> IO result) ->
  config ->
  IO result
queryGlobalIp' appLens sourceLens getIpFn config =
  case config ^. appLens of
    Many -> ShellApp.tryAppActions allApps
    Single app -> singleRun app
  where
    allApps =
      [ MkAppAction (singleRun GlobalIpDig) digSupported (showt GlobalIpDig),
        MkAppAction (singleRun GlobalIpCurl) curlSupported (showt GlobalIpCurl)
      ]
    singleRun a = getIpFn a (config ^. sourceLens)
{-# INLINEABLE queryGlobalIp' #-}

curlSupported :: IO Bool
curlSupported = U.exeSupported "curl"
{-# INLINEABLE curlSupported #-}

digSupported :: IO Bool
digSupported = U.exeSupported "dig"
{-# INLINEABLE digSupported #-}

getBothIps ::
  GlobalIpApp ->
  ([UrlSource 'Ipv4], [UrlSource 'Ipv6]) ->
  IO (IpAddress 'Ipv4, IpAddress 'Ipv6)
getBothIps app (ipv4Srcs, ipv6Srcs) =
  (,)
    <$> getIpv4s app ipv4Srcs
    <*> getIpv6s app ipv6Srcs
{-# INLINEABLE getBothIps #-}

getIpv4s ::
  GlobalIpApp ->
  [UrlSource 'Ipv4] ->
  IO (IpAddress 'Ipv4)
getIpv4s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv4Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources
{-# INLINEABLE getIpv4s #-}

getIpv6s ::
  GlobalIpApp ->
  [UrlSource 'Ipv6] ->
  IO (IpAddress 'Ipv6)
getIpv6s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv6Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources
{-# INLINEABLE getIpv6s #-}

prependApp :: GlobalIpApp -> [UrlSource a] -> [UrlSource a]
prependApp GlobalIpCurl srcs = fmap (#unUrlSource %~ ("curl " <>)) srcs
prependApp GlobalIpDig srcs = fmap (#unUrlSource %~ (\s -> "dig " <> s <> " +short")) srcs
{-# INLINEABLE prependApp #-}

ipv4Defaults :: GlobalIpApp -> [UrlSource 'Ipv4]
ipv4Defaults GlobalIpCurl = curlDefaults ^. _1
ipv4Defaults GlobalIpDig = digDefaults ^. _1
{-# INLINEABLE ipv4Defaults #-}

ipv6Defaults :: GlobalIpApp -> [UrlSource 'Ipv6]
ipv6Defaults GlobalIpCurl = curlDefaults ^. _2
ipv6Defaults GlobalIpDig = digDefaults ^. _2
{-# INLINEABLE ipv6Defaults #-}

curlDefaults :: ([UrlSource 'Ipv4], [UrlSource 'Ipv6])
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

digDefaults :: ([UrlSource 'Ipv4], [UrlSource 'Ipv6])
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

getIpFromSources :: Predicate (IpRefinement a) Text => [UrlSource a] -> IO (IpAddress a)
getIpFromSources = fmap MkIpAddress . getIp GIpTypes.urlSourceCmdIso
{-# INLINEABLE getIpFromSources #-}

getIp ::
  forall p a.
  Predicate p Text =>
  Iso' a Command ->
  [a] ->
  IO (Refined p Text)
getIp iso cmds = ShellApp.tryIOs (fmap go cmds)
  where
    go cmd = do
      txt <- ShellApp.runCommand $ cmd ^. iso
      R.refineThrow (trim txt)
{-# INLINEABLE getIp #-}

trim :: Text -> Text
trim = T.dropAround Char.isSpace
{-# INLINEABLE trim #-}
