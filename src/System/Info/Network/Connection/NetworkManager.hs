-- | This module provides functionality for retrieving network connection
-- information using NetworkManager.
module System.Info.Network.Connection.NetworkManager
  ( P.parseConnection,
    command,
  )
where

import System.Info.Data.Command (Command)
import System.Info.Network.Connection.NetworkManager.Parsing qualified as P

-- | This is the shell command that utilizes NetworkManager to retrieve
-- connection information.
command :: Command
command = "nmcli -m multiline device | cat"
