-- | This modules exports everything needed for retrieving global
-- IP addresses.
--
-- @since 0.1.0.0
module Pythia.Services.GlobalIP
  ( -- * Queries
    queryGlobalIp,
    queryGlobalIpConfig,

    -- * Types
    GlobalIpAddresses (..),
    Ipv4Address (..),
    Ipv6Address (..),

    -- ** Configuration
    GlobalIpConfig (..),
    GlobalIpApp (..),
    GlobalIpRequest (..),
    GlobalIpSources (..),
    UrlSource (..),
    IpType (..),
    RunApp (..),

    -- ** Errors
    GlobalIpException (..),
    uncheckGlobalIp,
    rethrowGlobalIp,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Optics.Core (Iso')
import Pythia.Data.Command (Command)
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Prelude
import Pythia.Services.GlobalIP.Types
  ( GlobalIpAddresses (..),
    GlobalIpApp (..),
    GlobalIpConfig (..),
    GlobalIpException (..),
    GlobalIpRequest (..),
    GlobalIpSources (..),
    UrlSource (..),
  )
import Pythia.Services.GlobalIP.Types qualified as GIpTypes
import Pythia.Services.Types (IpType (..), Ipv4Address (..), Ipv6Address (..))
import Pythia.ShellApp (AppAction (..), CmdException (..), MultiExceptions (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R

-- | Attempts to query for global ip addresses by detecting supported apps.
-- We try dig first, then curl.
--
-- @since 0.1.0.0
queryGlobalIp ::
  (MonadCatch m, MonadIO m, Throws GlobalIpException) => m GlobalIpAddresses
queryGlobalIp = queryGlobalIpConfig mempty

-- | Queries for global ip address based on the configuration.
--
-- @since 0.1.0.0
queryGlobalIpConfig ::
  (MonadCatch m, MonadIO m, Throws GlobalIpException) =>
  GlobalIpConfig ->
  m GlobalIpAddresses
queryGlobalIpConfig config =
  case config ^. #ipApp of
    Many -> rethrowGlobalIp @MultiExceptions $ ShellApp.tryAppActions allApps
    Single app -> singleRun app
  where
    allApps =
      [ MkAppAction (singleRun GlobalIpDig) digSupported (show GlobalIpDig),
        MkAppAction (singleRun GlobalIpCurl) curlSupported (show GlobalIpCurl)
      ]
    singleRun =
      toSingleShellApp
        (config ^. #ipRequestType)
        (config ^. #ipSources)

toSingleShellApp ::
  (MonadIO m, Throws GlobalIpException) =>
  GlobalIpRequest ->
  GlobalIpSources ->
  GlobalIpApp ->
  m GlobalIpAddresses
toSingleShellApp ipType (MkGlobalIpSources ipv4Srcs ipv6Srcs) app = liftIO $ do
  case ipType of
    GlobalIpRequestIpv4 -> getIpv4s app ipv4Srcs
    GlobalIpRequestIpv6 -> getIpv6s app ipv6Srcs
    GlobalIpRequestBoth -> getBoth app ipv4Srcs ipv6Srcs

curlSupported :: MonadIO m => m Bool
curlSupported = U.exeSupported "curl"

digSupported :: MonadIO m => m Bool
digSupported = U.exeSupported "dig"

getBoth ::
  Throws GlobalIpException =>
  GlobalIpApp ->
  [UrlSource 'Ipv4] ->
  [UrlSource 'Ipv6] ->
  IO GlobalIpAddresses
getBoth app ipv4Srcs ipv6Srcs = GIpBoth <$> getIpv4s' app ipv4Srcs <*> getIpv6s' app ipv6Srcs

getIpv4s ::
  Throws GlobalIpException =>
  GlobalIpApp ->
  [UrlSource 'Ipv4] ->
  IO GlobalIpAddresses
getIpv4s app srcs = do
  GIpv4 <$> getIpv4s' app srcs

getIpv4s' ::
  Throws GlobalIpException =>
  GlobalIpApp ->
  [UrlSource 'Ipv4] ->
  IO Ipv4Address
getIpv4s' app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv4Defaults app
        _ -> prependApp app extraSrcs
  getIpv4 sources

getIpv6s ::
  Throws GlobalIpException =>
  GlobalIpApp ->
  [UrlSource 'Ipv6] ->
  IO GlobalIpAddresses
getIpv6s app srcs = GIpv6 <$> getIpv6s' app srcs

getIpv6s' ::
  Throws GlobalIpException =>
  GlobalIpApp ->
  [UrlSource 'Ipv6] ->
  IO Ipv6Address
getIpv6s' app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv6Defaults app
        _ -> prependApp app extraSrcs
  getIpv6 sources

prependApp :: GlobalIpApp -> [UrlSource a] -> [UrlSource a]
prependApp GlobalIpCurl srcs = fmap (#unUrlSource %~ ("curl " <>)) srcs
prependApp GlobalIpDig srcs = fmap (#unUrlSource %~ (\s -> "dig " <> s <> " +short")) srcs

ipv4Defaults :: GlobalIpApp -> [UrlSource 'Ipv4]
ipv4Defaults GlobalIpCurl = curlDefaults ^. #ipv4Sources
ipv4Defaults GlobalIpDig = digDefaults ^. #ipv4Sources

ipv6Defaults :: GlobalIpApp -> [UrlSource 'Ipv6]
ipv6Defaults GlobalIpCurl = curlDefaults ^. #ipv6Sources
ipv6Defaults GlobalIpDig = digDefaults ^. #ipv6Sources

curlDefaults :: GlobalIpSources
curlDefaults = MkGlobalIpSources ipv4s ipv6s
  where
    ipv4s =
      [ "curl http://whatismyip.akamai.com/",
        "curl http://ifconfig.me/ip",
        "curl http://myexternalip.com/raw",
        "curl http://checkip.amazonaws.com/"
      ]
    ipv6s = []

digDefaults :: GlobalIpSources
digDefaults = MkGlobalIpSources ipv4s ipv6s
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

getIpv4 :: Throws GlobalIpException => [UrlSource 'Ipv4] -> IO Ipv4Address
getIpv4 = fmap MkIpv4Address . getIp GIpTypes.urlSourceCmdIso

getIpv6 :: Throws GlobalIpException => [UrlSource 'Ipv6] -> IO Ipv6Address
getIpv6 = fmap MkIpv6Address . getIp GIpTypes.urlSourceCmdIso

getIp ::
  forall p a.
  (Predicate p Text, Throws GlobalIpException) =>
  Iso' a Command ->
  [a] ->
  IO (Refined p Text)
getIp iso cmds =
  rethrowGlobalIp @MultiExceptions $ ShellApp.tryIOs (fmap go cmds)
  where
    go cmd = rethrowGlobalIp @CmdException $ do
      txt <- ShellApp.runCommand $ cmd ^. iso
      R.refineThrow (trim txt)

trim :: Text -> Text
trim = T.dropAround Char.isSpace

-- | 'uncheck' specialized to 'GlobalIpException'.
--
-- @since 0.1.0.0
uncheckGlobalIp :: ((Throws GlobalIpException) => m a) -> m a
uncheckGlobalIp = uncheck (Proxy @GlobalIpException)

-- | Rethrows a checked exception as a 'GlobalIpException'.
--
-- @since 0.1.0.0
rethrowGlobalIp :: forall e m a. (Exception e, MonadCatch m, Throws GlobalIpException) => (Throws e => m a) -> m a
rethrowGlobalIp = handle (\(ex :: e) -> throw $ MkGlobalIpErr ex)
