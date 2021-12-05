-- | This modules exports everything needed for retrieving global
-- IP addresses.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Global
  ( -- * Query
    queryGlobalIP,
    queryGlobalIPStrategy,

    -- * Types
    GlobalIpApp (..),

    -- ** IP Types
    GlobalIpAddresses (..),
    Ipv4,
    Ipv6,

    -- ** IP Commands
    IpStrategy (..),
    GlobalIpCommand (..),
    Ipv4Command (..),
    Ipv6Command (..),
  )
where

import Pythia.Services.Network.IP.Global.Common qualified as Common
import Pythia.Services.Network.IP.Global.Types
  ( GlobalIpAddresses (..),
    GlobalIpCommand (..),
    IpStrategy (..),
    Ipv4,
    Ipv4Command (..),
    Ipv6,
    Ipv6Command (..),
  )
import Pythia.ShellApp (QueryResult)
import Pythia.ShellApp qualified as ShellApp

-- | This type determines what program we use to lookup the ip address.
--
-- @since 0.1.0.0
data GlobalIpApp
  = -- | Uses the dig command to perform a DNS lookup. This is generally the
    -- fastest and most reliable.
    --
    -- @since 0.1.0.0
    GlobalDig
  | -- | Uses curl to lookup the ip addresses.
    --
    -- @since 0.1.0.0
    GlobalCurl
  | -- | Uses a custom command. As no default servers are used, this is
    -- equivalent to using one of the other commands (e.g. Dig) with
    -- an 'IpStrategy' of 'CustomUrl'. In particular, this means that using
    -- 'GlobalCustom' with 'Defaults' is effectively a no-op, as we will have
    -- no external sources to use. In short, use this command with
    -- 'queryGlobalIPStrategy' and 'CustomUrl'.
    --
    -- @since 0.1.0.0
    GlobalCustom
  deriving (Eq, Show)

-- | This is the primary function that attempts to use the given
-- program to retrieve network ip addresses. We use a variety of built-in
-- servers URLs to query our network information.
--
-- @since 0.1.0.0
queryGlobalIP :: GlobalIpApp -> IO (QueryResult GlobalIpAddresses)
queryGlobalIP = queryGlobalIPStrategy mempty

-- | Variant of 'queryGlobalIP' that takes in the 'IpStrategy'.
--
-- @since 0.1.0.0
queryGlobalIPStrategy :: IpStrategy -> GlobalIpApp -> IO (QueryResult GlobalIpAddresses)
queryGlobalIPStrategy strategy = \case
  GlobalDig -> ShellApp.runShellApp $ digShellApp strategy
  GlobalCurl -> ShellApp.runShellApp $ curlShellApp strategy
  GlobalCustom -> ShellApp.runShellApp $ Common.globalIpShellApp [] [] strategy

digShellApp :: IpStrategy -> ShellApp.ShellApp GlobalIpAddresses
digShellApp = Common.globalIpShellApp ipv4Cmds ipv6Cmds
  where
    ipv4Cmds =
      [ "dig @resolver1.opendns.com myip.opendns.com +short",
        "dig @resolver2.opendns.com myip.opendns.com +short",
        "dig @resolver3.opendns.com myip.opendns.com +short",
        "dig @resolver4.opendns.com myip.opendns.com +short",
        "dig @ns1-1.akamaitech.net ANY whoami.akamai.net +short",
        "dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com"
      ]
    ipv6Cmds = []

curlShellApp :: IpStrategy -> ShellApp.ShellApp GlobalIpAddresses
curlShellApp = Common.globalIpShellApp ipv4Cmds ipv6Cmds
  where
    ipv4Cmds =
      [ "curl http://whatismyip.akamai.com/",
        "curl http://ifconfig.me/ip",
        "curl http://myexternalip.com/raw",
        "curl http://checkip.amazonaws.com/"
      ]
    ipv6Cmds = []
