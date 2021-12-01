{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains the types for describing network connections.
--
-- @since 0.1.0.0
module System.Info.Services.Network.Connection.Types
  ( Connection (..),
    ConnType (..),
    ConnState (..),
  )
where

import Data.Text (Text)
import Optics.TH qualified as OTH
import System.Info.Services.Network.Types (Device)

-- | Various connection types.
--
-- @since 0.1.0.0
data ConnType
  = -- | @since 0.1.0.0
    Ethernet
  | -- | @since 0.1.0.0
    Wifi
  | -- | @since 0.1.0.0
    Wifi_P2P
  | -- | @since 0.1.0.0
    Loopback
  | -- | @since 0.1.0.0
    Tun
  | -- | @since 0.1.0.0
    UnknownType Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

OTH.makePrismLabels ''ConnType

-- | Various connection states.
data ConnState
  = -- | @since 0.1.0.0
    Connected
  | -- | @since 0.1.0.0
    Disconnected
  | -- | @since 0.1.0.0
    Unavailable
  | -- | @since 0.1.0.0
    Unmanaged
  | -- | @since 0.1.0.0
    UnknownState Text
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

OTH.makePrismLabels ''ConnState

-- | Full connection data.
--
-- @since 0.1.0.0
data Connection = MkConnection
  { -- | The device name.
    --
    -- @since 0.1.0.0
    device :: Device,
    -- | The connection type.
    --
    -- @since 0.1.0.0
    ctype :: ConnType,
    -- | The connection state.
    --
    -- @since 0.1.0.0
    state :: ConnState,
    -- | The name of the connection (e.g. Wifi SSID).
    --
    -- @since 0.1.0.0
    name :: Maybe Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

OTH.makeFieldLabelsNoPrefix ''Connection
