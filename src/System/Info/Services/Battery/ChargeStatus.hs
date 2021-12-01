-- | This modules exports everything needed for retrieving battery
-- charge status.
--
-- @since 0.1.0.0
module System.Info.Services.Battery.ChargeStatus
  ( -- * Types
    BatteryChargeStatusApp (..),
    ChargeStatus (..),

    -- * Query
    queryChargeStatus,
  )
where

import Data.Text (Text)
import Optics.Core ((%), (.~))
import System.Info.Data (Command (..))
import System.Info.Services.Battery.ChargeStatus.UPower qualified as UPower
import System.Info.Services.Battery.Types (ChargeStatus (..))
import System.Info.ShellApp (QueryResult, ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | Determines how we should query the system for charge status information.
-- The custom option assumes the same output format as UPower, i.e., the
-- output contains a line like:
--
-- @
-- state: \<discharging|charging|fully-charged\>
-- @
--
-- @since 0.1.0.0
data BatteryChargeStatusApp
  = -- | Uses the UPower utility.
    --
    -- @since 0.1.0.0
    UPower
  | -- | Runs a custom script.
    --
    -- @since 0.1.0.0
    Custom Text
  deriving
    ( -- @since 0.1.0.0
      Eq,
      -- @since 0.1.0.0
      Show
    )

-- | This is the primary function that attempts to use the given
-- BatteryChargeStatusApp to retrieve battery information.
--
-- >>> queryChargeStatus UPower
-- Right Discharging
--
-- @since 0.1.0.0
queryChargeStatus :: BatteryChargeStatusApp -> IO (QueryResult ChargeStatus)
queryChargeStatus UPower = ShellApp.runShellApp UPower.chargeStatusShellApp
queryChargeStatus (Custom c) = ShellApp.runShellApp $ customShellApp c

-- Reuse UPower's parser
customShellApp :: Text -> ShellApp ChargeStatus
customShellApp cmd =
  (#_SimpleApp % #command .~ MkCommand cmd)
    UPower.chargeStatusShellApp
