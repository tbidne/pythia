-- | This is the entry point to the library. All services are exported
-- here. See the modules themselves for more documentation.
--
-- @since 0.1
module Pythia
  ( -- * Services
    -- $services
    module Pythia.Services.Battery,
    module Pythia.Services.GlobalIp,
    module Pythia.Services.NetInterface,

    -- * Exceptions
    -- $exceptions
    module Pythia.Control.Exception,

    -- * Printing
    -- $printing
    module Pythia.Class.Printer,
  )
where

import Pythia.Class.Printer
import Pythia.Control.Exception
import Pythia.Services.Battery
import Pythia.Services.GlobalIp
import Pythia.Services.NetInterface

-- $services
-- Each service is self-contained in that it should have everything you need
-- to use it.

-- $exceptions
-- Pythia's error handling is defined in terms of @safe-exceptions@.
-- "Pythia.Control.Exception" defines general exceptions that can be thrown
-- from 'GHC.IO.IO'; Additionally, services can also throw specific exceptions (e.g.
-- 'AcpiException' from "Pythia.Services.Battery.Acpi"). All exceptions are
-- unifed under the supertype 'PythiaException'.

-- $printing
-- This module contains a typeclass used for pretty printing various types.
