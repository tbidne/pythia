-- | This modules exports everything needed for retrieving global
-- IP addresses.
--
-- @since 0.1.0.0
module Pythia.Services.Network.GlobalIP
  ( -- * Queries
    queryGlobalIp,
    queryGlobalIpConfig,

    -- * Types
    module Pythia.Services.Network.GlobalIP.Types,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Optics.Core (Iso')
import Pythia.Data (Command, QueryError, RunApp (..))
import Pythia.Data qualified as Data
import Pythia.Prelude
import Pythia.Services.Network.GlobalIP.Types
  ( GlobalIpAddresses (..),
    GlobalIpApp (..),
    GlobalIpConfig (..),
    GlobalIpRequest (..),
    GlobalIpSources (..),
    UrlSource (..),
  )
import Pythia.Services.Network.GlobalIP.Types qualified as GIpTypes
import Pythia.Services.Network.Types (IpType (..), Ipv4Address (..), Ipv6Address (..))
import Pythia.ShellApp (GeneralShell (..), QueryResult, ShellApp (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R

-- | Attempts to query for global ip addresses by detecting supported apps.
-- We try dig first, then curl.
--
-- @since 0.1.0.0
queryGlobalIp :: IO (QueryResult GlobalIpAddresses)
queryGlobalIp = queryGlobalIpConfig mempty

-- | Attempts to query for global ip addresses by detecting supported apps.
-- We try dig first, then curl.
--
-- @since 0.1.0.0
queryGlobalIpConfig :: GlobalIpConfig -> IO (QueryResult GlobalIpAddresses)
queryGlobalIpConfig config =
  case config ^. #ipApp of
    Many -> ShellApp.tryIOs allApps
    Single app -> singleRun app
  where
    allApps =
      [ (singleRun GlobalIpDig, digSupported),
        (singleRun GlobalIpCurl, curlSupported)
      ]
    singleRun =
      ShellApp.runShellApp
        . toSingleShellApp
          (config ^. #ipRequestType)
          (config ^. #ipSources)

toSingleShellApp :: GlobalIpRequest -> GlobalIpSources -> GlobalIpApp -> ShellApp GlobalIpAddresses
toSingleShellApp ipType (MkGlobalIpSources ipv4Srcs ipv6Srcs) app =
  GeneralApp $
    MkGeneralShell $ do
      case ipType of
        GlobalIpRequestIpv4 -> getIpv4s app ipv4Srcs
        GlobalIpRequestIpv6 -> getIpv6s app ipv6Srcs
        GlobalIpRequestBoth -> getBoth app ipv4Srcs ipv6Srcs

curlSupported :: IO Bool
curlSupported = U.exeSupported "curl"

digSupported :: IO Bool
digSupported = U.exeSupported "dig"

getBoth :: GlobalIpApp -> [UrlSource 'Ipv4] -> [UrlSource 'Ipv6] -> IO (QueryResult GlobalIpAddresses)
getBoth app ipv4Srcs ipv6Srcs = do
  x <- getIpv4s' app ipv4Srcs
  y <- getIpv6s' app ipv6Srcs
  pure $ GIpBoth <$> x <*> y

getIpv4s :: GlobalIpApp -> [UrlSource 'Ipv4] -> IO (QueryResult GlobalIpAddresses)
getIpv4s app srcs = (_Right %~ GIpv4) <$> getIpv4s' app srcs

getIpv4s' :: GlobalIpApp -> [UrlSource 'Ipv4] -> IO (QueryResult Ipv4Address)
getIpv4s' app extraSrcs = do
  let sources = case extraSrcs of
        [] -> ipv4Defaults app
        _ -> prependApp app extraSrcs
  getIpv4 sources

getIpv6s :: GlobalIpApp -> [UrlSource 'Ipv6] -> IO (QueryResult GlobalIpAddresses)
getIpv6s app srcs = (_Right %~ GIpv6) <$> getIpv6s' app srcs

getIpv6s' :: GlobalIpApp -> [UrlSource 'Ipv6] -> IO (QueryResult Ipv6Address)
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

getIpv4 :: [UrlSource 'Ipv4] -> IO (Either [QueryError] Ipv4Address)
getIpv4 = fmap (_Right %~ MkIpv4Address) . getIp GIpTypes.urlSourceCmdIso

getIpv6 :: [UrlSource 'Ipv6] -> IO (Either [QueryError] Ipv6Address)
getIpv6 = fmap (_Right %~ MkIpv6Address) . getIp GIpTypes.urlSourceCmdIso

getIp ::
  Predicate p Text =>
  Iso' a Command ->
  [a] ->
  IO (Either [QueryError] (Refined p Text))
getIp iso = foldr go (pure (Left []))
  where
    go cmd acc = do
      res <- ShellApp.runCommand $ cmd ^. iso
      case res of
        Left err -> appendErr err <$> acc
        Right txt -> case R.refine (trim txt) of
          Left refEx ->
            appendErr (Data.refineExToQueryError refEx) <$> acc
          Right ip -> pure $ Right ip
    appendErr e = _Left %~ (e :)

trim :: Text -> Text
trim = T.dropAround Char.isSpace
