-- | This module exports network related services.
--
-- @since 0.1.0.0
module System.Info.Services.Network
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
    Ipv4,
    Ipv6,

    -- * Misc Types
    QueryResult,
    QueryError (..),
  )
where

import System.Info.Data (QueryError (..))
import System.Info.Services.Network.Connection
  ( ConnState (..),
    ConnType (..),
    Connection (..),
    Device (..),
    NetConnApp (..),
    queryConnection,
  )
import System.Info.Services.Network.IP.Global
  ( GlobalIpAddresses (..),
    GlobalIpApp (..),
    GlobalIpCommand (..),
    IpStrategy (..),
    Ipv4,
    Ipv4Command (..),
    Ipv6,
    Ipv6Command (..),
    queryGlobalIP,
    queryGlobalIPStrategy,
  )
import System.Info.Services.Network.IP.Local
  ( LocalIpApp (..),
    LocalIps (..),
    queryLocalIP,
  )
import System.Info.ShellApp (QueryResult)
