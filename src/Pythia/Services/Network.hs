-- | This module exports network related services.
--
-- @since 0.1.0.0
module Pythia.Services.Network
  ( -- * Services

    -- ** Connection
    queryConnection,
    NetConnApp (..),
    Connection (..),
    ConnType (..),
    ConnState (..),

    -- ** IP Addresses

    -- *** Local
    queryLocalIP,
    LocalIpApp (..),
    LocalIps (..),

    -- *** Global
    queryGlobalIP,
    queryGlobalIPStrategy,
    GlobalIpApp (..),
    GlobalIpAddresses (..),
    IpStrategy (..),
    GlobalIpCommand (..),
    Ipv4Command (..),
    Ipv6Command (..),

    -- ** Common Types
    Device (..),
    Ipv4 (..),
    Ipv6 (..),

    -- * Misc Types
    QueryResult,
    QueryError (..),
  )
where

import Pythia.Data (QueryError (..))
import Pythia.Services.Network.Connection
  ( ConnState (..),
    ConnType (..),
    Connection (..),
    Device (..),
    NetConnApp (..),
    queryConnection,
  )
import Pythia.Services.Network.IP.Global
  ( GlobalIpAddresses (..),
    GlobalIpApp (..),
    GlobalIpCommand (..),
    IpStrategy (..),
    Ipv4Command (..),
    Ipv6Command (..),
    queryGlobalIP,
    queryGlobalIPStrategy,
  )
import Pythia.Services.Network.IP.Local
  ( LocalIpApp (..),
    LocalIps (..),
    queryLocalIP,
  )
import Pythia.Services.Network.IP.Types (Ipv4 (..), Ipv6 (..))
import Pythia.ShellApp (QueryResult)
