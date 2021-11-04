-- | This modules exports everything needed for retrieving battery
-- level and charge status.
module System.Info.Services.Battery.State
  ( -- * Types
    Program (..),
    BatteryState (..),
    BatteryLevel,
    BoundedNat (..),
    ChargeStatus (..),

    -- * Query
    queryBatteryState,
  )
where

import Data.Text (Text)
import Optics.Core ((^.))
import Smart.Data.Math.BoundedNat (BoundedNat (..))
import System.Info.Data (Command (..), QueryError)
import System.Info.Services.Battery.State.UPower qualified as UPower
import System.Info.Services.Battery.Types
  ( BatteryLevel,
    BatteryState (..),
    ChargeStatus (..),
  )
import System.Info.ShellApp (ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | Determines how we should query the system for battery state information.
-- The custom option assumes the same output format as UPower, i.e., the
-- output contains lines like:
--
-- @
-- percentage: 20%
-- state: \<discharging|charging|fully-charged\>
-- @
data Program
  = -- | Uses the UPower utility.
    UPower
  | -- | Runs a custom script.
    Custom Text
  deriving (Eq, Show)

-- | This is the primary function that attempts to use the given
-- program to retrieve battery information.
--
-- >>> queryBatteryState UPower
-- Right (MkBatteryState {level = MkUnsafeBoundedNat {unBoundedNat = 24}, status = Charging})
queryBatteryState :: Program -> IO (Either QueryError BatteryState)
queryBatteryState UPower = ShellApp.runShellApp UPower.batteryStateShellApp
queryBatteryState (Custom c) = ShellApp.runShellApp $ customShellApp c

customShellApp :: Text -> ShellApp BatteryState
customShellApp cmd =
  MkShellApp
    { command = MkCommand cmd,
      -- Reuse UPower's parser
      parser = UPower.batteryStateShellApp ^. #parser
    }
