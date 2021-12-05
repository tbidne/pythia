-- | This modules exports everything needed for retrieving network
-- connection information.
--
-- @since 0.1.0.0
module Pythia.Services.Network.Connection
  ( -- * Query
    queryConnection,

    -- * Types
    NetConnApp (..),
    Connection (..),
    ConnType (..),
    ConnState (..),
    Device (..),
  )
where

import Data.Text (Text)
import Optics.Core ((%), (.~))
import Pythia.Data (Command (..))
import Pythia.Services.Network.Connection.NmCli qualified as NM
import Pythia.Services.Network.Connection.Types
  ( ConnState (..),
    ConnType (..),
    Connection (..),
  )
import Pythia.Services.Network.Types (Device (..))
import Pythia.ShellApp (QueryResult, ShellApp (..))
import Pythia.ShellApp qualified as ShellApp

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
    NetConNmCli Device
  | -- | Custom command.
    --
    -- @since 0.1.0.0
    NetConCustom Device Text
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
-- Right (MkConnection {device = MkDevice {unDevice = "wlp0s20f3"}, connType = Wifi, state = Connected, name = Just "Some SSID"})
--
-- @since 0.1.0.0
queryConnection :: NetConnApp -> IO (QueryResult Connection)
queryConnection = \case
  NetConNmCli device -> ShellApp.runShellApp $ NM.connectionShellApp device
  NetConCustom device c -> ShellApp.runShellApp $ customShellApp device c

-- Reuse NmCli's parser
customShellApp :: Device -> Text -> ShellApp Connection
customShellApp device cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    (NM.connectionShellApp device)
