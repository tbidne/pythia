-- | This is the entry point to the library. All services are exported
-- here. See the modules themselves for more documentation.
--
-- @since 0.1.0.0
module Pythia
  ( -- * Services
    module Pythia.Services.Battery,
    module Pythia.Services.GlobalIP,
    module Pythia.Services.NetInterface,

    -- * Data
    module Pythia.Data.Command,
    module Pythia.Data.RunApp,

    -- * Printing
    module Pythia.Printer,
  )
where

import Pythia.Data.Command
import Pythia.Data.RunApp
import Pythia.Printer
import Pythia.Services.Battery
import Pythia.Services.GlobalIP
import Pythia.Services.NetInterface
