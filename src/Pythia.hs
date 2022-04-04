-- | This is the entry point to the library. All services are exported
-- here. See the modules themselves for more documentation.
--
-- @since 0.1.0.0
module Pythia
  ( -- * Services
    -- $services
    module Pythia.Services.Battery,
    module Pythia.Services.GlobalIP,
    module Pythia.Services.NetInterface,

    -- * Exceptions
    -- $exceptions
    module Pythia.Control.Exception,

    -- * Printing
    -- $printing
    module Pythia.Printer,
  )
where

import Pythia.Control.Exception
import Pythia.Printer
import Pythia.Services.Battery
import Pythia.Services.GlobalIP
import Pythia.Services.NetInterface

-- $setup
-- >>> import Pythia.Prelude (IO, Throws, pure)

-- $services
-- Each service is self-contained in that it should have everything you need
-- to use it.

-- $exceptions
-- Pythia's error handling is defined in terms of @safe-exceptions-checked@.
-- That is, service functions (e.g. 'queryBattery') either return a result
-- or throw a checked exception. Each service has a function that allows one
-- to swallow such an exception for convenience e.g.
--
-- >>> :{
-- -- without 'uncheckBattery' we would have to include the error i.e.
-- -- getBatteryUnchecked :: Throws BatteryException => IO Battery
-- getBatteryUnchecked :: IO Battery
-- getBatteryUnchecked = uncheckBattery queryBattery
-- :}
--
-- Additionally, we define these service exceptions in terms of a
-- 'Pythia.Control.Exception.PythiaException' supertype, defined in
-- "Pythia.Control.Exception". This is the root of our exception
-- hierarchy. This is not necessary to work with any individual service,
-- though it can be used to unify exceptions if, say, you would like to run
-- multiple services and not have to mention every error. For example:
--
-- >>> :{
--   -- Notice our signature mentions PythiaException but not BatteryException nor
--   -- NetInterfacesException.
--   batteryAndNetInterfaces :: Throws PythiaException => IO (Battery, NetInterfaces)
--   batteryAndNetInterfaces = do
--     battery <- rethrowPythia @BatteryException queryBattery
--     netInterfaces <- rethrowPythia @NetInterfaceException queryNetInterfaces
--     pure (battery, netInterfaces)
-- :}
--
-- Naturally, this can also be unchecked:
--
-- >>> :{
--   batteryAndNetInterfacesUnchecked :: IO (Battery, NetInterfaces)
--   batteryAndNetInterfacesUnchecked = uncheckPythia batteryAndNetInterfaces
-- :}

-- $printing
-- This module contains a typeclass used for pretty printing various types.
