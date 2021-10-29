-- | This modules exports everything needed for retrieving network
-- connection information.
module System.Info.Network.Connection
  ( module X,
    queryConnection,
  )
where

import Data.Text (Text)
import System.Info.Data.Command (Command (..))
import System.Info.Data.Error (Error)
import System.Info.Network.Connection.NetworkManager qualified as NM
import System.Info.Network.Connection.Types as X
import System.Info.Utils qualified as U

-- | This is the primary function that attempts to use the given
-- program to retrieve network connection information.
queryConnection :: Text -> ConnectionProgram -> IO (Either Error Connection)
queryConnection deviceName = \case
  NetworkManager -> U.runShellAndParse parseDevice NM.command
  (Custom c) -> U.runShellAndParse parseDevice (MkCommand c)
  where
    parseDevice = NM.parseConnection deviceName
