-- | This module contains the types for describing network connections.
module System.Info.Services.Network.Connection.Types
  ( Device (..),
    Connection (..),
    ConnType (..),
    ConnState (..),
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Optics.Core (A_Lens, LabelOptic (..))
import Optics.Core qualified as O

-- | Newtype wrapper over a network device name.
newtype Device = MkDevice {unDevice :: Text}
  deriving (Eq, Ord, Show)
  deriving (IsString) via Text

instance LabelOptic "unDevice" A_Lens Device Device Text Text where
  labelOptic = O.lens unDevice (\device t -> device {unDevice = t})

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

-- | Full connection data.
data Connection = MkConnection
  { -- | The device name.
    device :: Device,
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
    Device
    Device
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
