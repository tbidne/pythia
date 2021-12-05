-- | This modules exports everything needed for retrieving local
-- IP addresses for a given device.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Local
  ( -- * Query
    queryLocalIP,

    -- * Types
    LocalIpApp (..),
    LocalIps (..),
    LocalIpAddresses (..),
    Ipv4,
    Ipv6,
  )
where

import Data.Text (Text)
import Optics.Core ((%), (.~))
import Pythia.Data (Command (..))
import Pythia.Services.Network.IP.Local.IfConfig qualified as IfConfig
import Pythia.Services.Network.IP.Local.NmCli qualified as NM
import Pythia.Services.Network.IP.Local.Types
  ( LocalIpAddresses (..),
    LocalIps (..),
  )
import Pythia.Services.Network.IP.Types (Ipv4, Ipv6)
import Pythia.Services.Network.Types (Device (..))
import Pythia.ShellApp (QueryResult, ShellApp (..))
import Pythia.ShellApp qualified as ShellApp

-- | The program used to retrieve local IP addresses.
--
-- @since 0.1.0.0
data LocalIpApp
  = -- | Uses ifconfig.
    --
    -- @since 0.1.0.0
    LocalIfConfig Device
  | -- | Uses the NetworkManager (nmcli) utility.
    --
    -- @since 0.1.0.0
    LocalNmCli Device
  | -- | Custom command.
    --
    -- @since 0.1.0.0
    LocalCustom Device Text
  deriving (Eq, Show)

-- | This is the primary function that attempts to use the given
-- program to retrieve network ip addresses.
--
-- @since 0.1.0.0
queryLocalIP :: LocalIpApp -> IO (QueryResult LocalIps)
queryLocalIP = \case
  LocalIfConfig device -> ShellApp.runShellApp $ IfConfig.ipShellApp device
  LocalNmCli device -> ShellApp.runShellApp $ NM.ipShellApp device
  LocalCustom device c -> ShellApp.runShellApp $ customShellApp device c

-- Reuse ifconfig's parser
customShellApp :: Device -> Text -> ShellApp LocalIps
customShellApp device cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    (IfConfig.ipShellApp device)
