-- | This is the entry point to the library. All services are exported
-- here. See the modules themselves for more documentation.
--
-- @since 0.1
module Pythia
  ( -- * Services
    -- $services
    module Pythia.Services.Battery,
    module Pythia.Services.GlobalIp,
    module Pythia.Services.Memory,
    module Pythia.Services.NetInterface,
    module Pythia.Services.Time,

    -- * Exceptions
    -- $exceptions
    module Pythia.Control.Exception,
  )
where

import Pythia.Control.Exception
import Pythia.Services.Battery
import Pythia.Services.GlobalIp
import Pythia.Services.Memory
import Pythia.Services.NetInterface
import Pythia.Services.Time

-- $services
-- Each service is self-contained in that it should have everything you need
-- to use it.

-- $exceptions
-- Pythia's error handling is defined in terms of @safe-exceptions@.
-- "Pythia.Control.Exception" defines general exceptions that can be thrown
-- from 'GHC.IO.IO'; Additionally, services can also throw specific exceptions (e.g.
-- 'AcpiException' from "Pythia.Services.Battery.Acpi"). All exceptions are
-- unifed under the supertype 'PythiaException'.
