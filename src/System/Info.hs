-- | This is the entry point to the library. All services are exported
-- here. See the modules themselves for more documentation.
--
-- @since 0.1.0.0
module System.Info
  ( -- * Services
    module System.Info.Services.Battery,
    module System.Info.Services.Network,
  )
where

import System.Info.Services.Battery
import System.Info.Services.Network
