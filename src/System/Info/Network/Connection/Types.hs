-- | This module contains the types for describing network connections.
module System.Info.Network.Connection.Types
  ( Connection (..),
    ConnType (..),
    ConnState (..),
    ConnectionProgram (..),
  )
where

import Data.Text (Text)
import Optics.Core (A_Lens, LabelOptic (..))
import Optics.Core qualified as O

-- | Various connection types.
data ConnType
  = Ethernet
  | Wifi
  | Wifi_P2P
  | Loopback
  | Tun
  | UnknownType Text
  deriving (Eq, Show)

-- | Various connection states.
data ConnState
  = Connected
  | Disconnected
  | Unavailable
  | Unmanaged
  | UnknownState Text
  deriving (Eq, Show)

-- | Determines how we should query the system for network information.
data ConnectionProgram
  = -- | Uses the NetworkManager utility.
    NetworkManager
  | -- | Custom command.
    Custom Text
  deriving (Show)

-- | Full connection data.
data Connection = MkConnection
  { -- | The device name.
    device :: Text,
    -- | The connection type.
    ctype :: ConnType,
    -- | The connection state.
    state :: ConnState,
    -- | The name of the connection (e.g. Wifi SSID).
    name :: Maybe Text
  }
  deriving (Eq, Show)

instance
  LabelOptic
    "device"
    A_Lens
    Connection
    Connection
    Text
    Text
  where
  labelOptic = O.lens device (\conn device' -> conn {device = device'})

instance
  LabelOptic
    "ctype"
    A_Lens
    Connection
    Connection
    ConnType
    ConnType
  where
  labelOptic = O.lens ctype (\conn ctype' -> conn {ctype = ctype'})

instance
  LabelOptic
    "state"
    A_Lens
    Connection
    Connection
    ConnState
    ConnState
  where
  labelOptic = O.lens state (\conn state' -> conn {state = state'})

instance
  LabelOptic
    "name"
    A_Lens
    Connection
    Connection
    (Maybe Text)
    (Maybe Text)
  where
  labelOptic = O.lens name (\conn name' -> conn {name = name'})
