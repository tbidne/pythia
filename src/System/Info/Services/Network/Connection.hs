-- | This modules exports everything needed for retrieving network
-- connection information.
--
-- @since 0.1.0.0
module System.Info.Services.Network.Connection
  ( -- * Types
    NetConnApp (..),
    Connection (..),
    ConnType (..),
    ConnState (..),
    Device (..),

    -- * Query
    queryConnection,
  )
where

import Data.Text (Text)
import Optics.Core ((%), (.~))
import System.Info.Data (Command (..))
import System.Info.Services.Network.Connection.NmCli qualified as NM
import System.Info.Services.Network.Connection.Types
  ( ConnState (..),
    ConnType (..),
    Connection (..),
  )
import System.Info.Services.Network.Types (Device (..))
import System.Info.ShellApp (QueryResult, ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | Determines how we should query the system for network information.
-- The custom option assumes the same output format as NmCli, i.e.,
-- the output contains lines like:
--
-- @
-- DEVICE:       wlp0s20f3
-- TYPE:         wifi
-- STATE:        connected
-- CONNECTION:   Some SSID
-- @
--
-- @since 0.1.0.0
data NetConnApp
  = -- | Uses the NmCli utility.
    --
    -- @since 0.1.0.0
    NmCli Device
  | -- | Custom command.
    --
    -- @since 0.1.0.0
    Custom Device Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | This is the primary function that attempts to use the given
-- NetConnApp to retrieve network connection information.
--
-- >>> queryConnection (NmCli "wlp0s20f3")
-- Right (MkConnection {device = MkDevice {unDevice = "wlp0s20f3"}, ctype = Wifi, state = Connected, name = Just "Some SSID"})
--
-- | @since 0.1.0.0
queryConnection :: NetConnApp -> IO (QueryResult Connection)
queryConnection = \case
  NmCli device -> ShellApp.runShellApp $ NM.connectionShellApp device
  Custom device c -> ShellApp.runShellApp $ customShellApp device c

-- Reuse NmCli's parser
customShellApp :: Device -> Text -> ShellApp Connection
customShellApp device cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    (NM.connectionShellApp device)
