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
import Pythia.Data.Command (Command, _MkCommand)
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Internal.ShellApp (AppAction (..))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.GlobalIp.Types
  ( GlobalIpApp (..),
    GlobalIpBothConfig,
    GlobalIpConfig (..),
    GlobalIpv4Config,
    GlobalIpv6Config,
    UrlSource (..),
    _MkUrlSource,
  )
import Pythia.Services.Types.Network
  ( IpAddress (..),
    IpRefinement,
    IpType (..),
  )
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R

-- | Queries for IPv4 and IPv6 global IP address based on the configuration.
-- If 'app' is 'RunAppMany' then we try supported apps in the following
-- order:
--
-- @
-- ['GlobalIpAppDig', 'GlobalIpAppCurl']
-- @
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.CommandException'
--
-- @since 0.1
queryGlobalIp :: GlobalIpBothConfig -> IO (IpAddress 'IpTypeIpv4, IpAddress 'IpTypeIpv6)
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
queryGlobalIpv4 :: GlobalIpv4Config -> IO (IpAddress 'IpTypeIpv4)
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
queryGlobalIpv6 :: GlobalIpv6Config -> IO (IpAddress 'IpTypeIpv6)
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
    RunAppMany -> ShellApp.tryAppActions allApps
    RunAppSingle app -> singleRun app
  where
    allApps =
      [ MkAppAction (singleRun GlobalIpAppDig) digSupported "dig",
        MkAppAction (singleRun GlobalIpAppCurl) curlSupported "curl"
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
  ([UrlSource 'IpTypeIpv4], [UrlSource 'IpTypeIpv6]) ->
  IO (IpAddress 'IpTypeIpv4, IpAddress 'IpTypeIpv6)
getBothIps app (ipv4Srcs, ipv6Srcs) =
  (,)
    <$> getIpv4s app ipv4Srcs
    <*> getIpv6s app ipv6Srcs
{-# INLINEABLE getBothIps #-}

getIpv4s ::
  GlobalIpApp ->
  [UrlSource 'IpTypeIpv4] ->
  IO (IpAddress 'IpTypeIpv4)
getIpv4s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv4Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources
{-# INLINEABLE getIpv4s #-}

getIpv6s ::
  GlobalIpApp ->
  [UrlSource 'IpTypeIpv6] ->
  IO (IpAddress 'IpTypeIpv6)
getIpv6s app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv6Defaults app
        _ -> prependApp app extraSrcs
  getIpFromSources sources
{-# INLINEABLE getIpv6s #-}

prependApp :: GlobalIpApp -> [UrlSource a] -> [UrlSource a]
prependApp GlobalIpAppCurl srcs = fmap (_MkUrlSource %~ ("curl " <>)) srcs
prependApp GlobalIpAppDig srcs = fmap (_MkUrlSource %~ (\s -> "dig " <> s <> " +short")) srcs
{-# INLINEABLE prependApp #-}

ipv4Defaults :: GlobalIpApp -> [UrlSource 'IpTypeIpv4]
ipv4Defaults GlobalIpAppCurl = curlDefaults ^. _1
ipv4Defaults GlobalIpAppDig = digDefaults ^. _1
{-# INLINEABLE ipv4Defaults #-}

ipv6Defaults :: GlobalIpApp -> [UrlSource 'IpTypeIpv6]
ipv6Defaults GlobalIpAppCurl = curlDefaults ^. _2
ipv6Defaults GlobalIpAppDig = digDefaults ^. _2
{-# INLINEABLE ipv6Defaults #-}

curlDefaults :: ([UrlSource 'IpTypeIpv4], [UrlSource 'IpTypeIpv6])
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

digDefaults :: ([UrlSource 'IpTypeIpv4], [UrlSource 'IpTypeIpv6])
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
getIpFromSources = fmap MkIpAddress . getIp (_MkUrlSource % re _MkCommand)
{-# INLINEABLE getIpFromSources #-}

getIp ::
  forall p a.
  Predicate p Text =>
  Iso' a Command ->
  [a] ->
  IO (Refined p Text)
getIp cmdIso cmds = ShellApp.tryIOs (fmap go cmds)
  where
    go cmd = do
      txt <- ShellApp.runCommand $ cmd ^. cmdIso
      R.refineThrow (trim txt)
{-# INLINEABLE getIp #-}

trim :: Text -> Text
trim = T.dropAround Char.isSpace
{-# INLINEABLE trim #-}
