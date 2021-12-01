-- | This modules exports everything needed for retrieving local
-- IP addresses for a given device.
--
-- @since 0.1.0.0
module System.Info.Services.Network.IP.Local
  ( queryLocalIP,
    LocalIPApp (..),
  )
where

import Data.Text (Text)
import Optics.Core ((%), (.~))
import System.Info.Data (Command (..))
import System.Info.Services.Network.IP.Local.IfConfig qualified as IfConfig
import System.Info.Services.Network.IP.Local.NmCli qualified as NM
import System.Info.Services.Network.IP.Local.Types (LocalIps)
import System.Info.Services.Network.Types (Device (..))
import System.Info.ShellApp (QueryResult, ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | The program used to retrieve local IP addresses.
--
-- @since 0.1.0.0
data LocalIPApp
  = -- | Uses ifconfig.
    --
    -- @since 0.1.0.0
    IfConfig Device
  | -- | Uses the NetworkManager (nmcli) utility.
    --
    -- @since 0.1.0.0
    NmCli Device
  | -- | Custom command.
    --
    -- @since 0.1.0.0
    Custom Device Text
  deriving (Eq, Show)

-- | This is the primary function that attempts to use the given
-- program to retrieve network ip addresses.
--
-- @since 0.1.0.0
queryLocalIP :: LocalIPApp -> IO (QueryResult LocalIps)
queryLocalIP = \case
  IfConfig device -> ShellApp.runShellApp $ IfConfig.ipShellApp device
  NmCli device -> ShellApp.runShellApp $ NM.ipShellApp device
  Custom device c -> ShellApp.runShellApp $ customShellApp device c

-- Reuse ifconfig's parser
customShellApp :: Device -> Text -> ShellApp LocalIps
customShellApp device cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    (IfConfig.ipShellApp device)
