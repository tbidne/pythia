-- | This modules exports everything needed for retrieving network
-- connection information.
module System.Info.Services.Network.Connection
  ( -- * Types
    Program (..),
    Connection (..),
    ConnType (..),
    ConnState (..),
    Device (..),

    -- * Query
    queryConnection,
  )
where

import Data.Text (Text)
import Optics.Core ((^.))
import System.Info.Data (Command (..), QueryError)
import System.Info.Services.Network.Connection.NetworkManager qualified as NM
import System.Info.Services.Network.Connection.Types
  ( ConnState (..),
    ConnType (..),
    Connection (..),
    Device (..),
  )
import System.Info.ShellApp (ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | Determines how we should query the system for network information.
-- The custom option assumes the same output format as NetworkManager, i.e.,
-- the output contains lines like:
--
-- @
-- DEVICE:       wlp0s20f3
-- TYPE:         wifi
-- STATE:        connected
-- CONNECTION:   Some SSID
-- @
data Program
  = -- | Uses the NetworkManager utility.
    NetworkManager Device
  | -- | Custom command.
    Custom Device Text
  deriving (Eq, Show)

-- | This is the primary function that attempts to use the given
-- program to retrieve network connection information.
--
-- >>> queryConnection (NetworkManager "wlp0s20f3")
-- Right (MkConnection {device = MkDevice {unDevice = "wlp0s20f3"}, ctype = Wifi, state = Connected, name = Just "Some SSID"})
queryConnection :: Program -> IO (Either QueryError Connection)
queryConnection = \case
  NetworkManager device -> ShellApp.runShellApp $ NM.connectionShellApp device
  Custom device c -> ShellApp.runShellApp $ customShellApp device c

customShellApp :: Device -> Text -> ShellApp Connection
customShellApp device cmd =
  MkShellApp
    { command = MkCommand cmd,
      -- Reuse NetworkManager's parser
      parser = NM.connectionShellApp device ^. #parser
    }
