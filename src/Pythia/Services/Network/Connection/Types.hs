{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains the types for describing network connections.
--
-- @since 0.1.0.0
module Pythia.Services.Network.Connection.Types
  ( Connection (..),
    ConnType (..),
    ConnState (..),
  )
where

import Data.Text qualified as T
import Optics.TH qualified as OTH
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Printer qualified as Printer
import Pythia.Services.Network.Types (Device)

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
  deriving anyclass
    ( -- | @since 0.1.0.0
      PrettyPrinter
    )

OTH.makePrismLabels ''ConnType

-- | Various connection states.
--
-- @since 0.1.0.0
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
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      PrettyPrinter
    )

OTH.makePrismLabels ''ConnState

-- | Full connection data.
--
-- @since 0.1.0.0
data Connection = MkConnection
  { -- | @since 0.1.0.0
    connDevice :: Device,
    -- | @since 0.1.0.0
    connType :: ConnType,
    -- | @since 0.1.0.0
    connState :: ConnState,
    -- | The name of the connection (e.g. Wifi SSID).
    --
    -- @since 0.1.0.0
    connName :: Maybe Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

OTH.makeFieldLabelsNoPrefix ''Connection

-- | @since 0.1.0.0
instance PrettyPrinter Connection where
  pretty conn =
    Printer.joinNewlines
      [ device,
        ctype,
        state,
        name
      ]
    where
      device = "Device: " <> pretty (conn ^. #connDevice)
      ctype = "Type: " <> pretty (conn ^. #connType)
      state = "State: " <> pretty (conn ^. #connState)
      name = "Name: " <> maybe "<None>" T.unpack (conn ^. #connName)
