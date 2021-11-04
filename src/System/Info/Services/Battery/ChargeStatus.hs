-- | This modules exports everything needed for retrieving battery
-- charge status.
module System.Info.Services.Battery.ChargeStatus
  ( -- * Types
    Program (..),
    ChargeStatus (..),

    -- * Query
    queryChargeStatus,
  )
where

import Data.Text (Text)
import Optics.Core ((^.))
import System.Info.Data (Command (..), QueryError)
import System.Info.Services.Battery.ChargeStatus.UPower qualified as UPower
import System.Info.Services.Battery.Types (ChargeStatus (..))
import System.Info.ShellApp (ShellApp (..))
import System.Info.ShellApp qualified as ShellApp

-- | Determines how we should query the system for charge status information.
-- The custom option assumes the same output format as UPower, i.e., the
-- output contains a line like:
--
-- @
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
-- >>> queryChargeStatus UPower
-- Right Discharging
queryChargeStatus :: Program -> IO (Either QueryError ChargeStatus)
queryChargeStatus UPower = ShellApp.runShellApp UPower.chargeStatusShellApp
queryChargeStatus (Custom c) = ShellApp.runShellApp $ customShellApp c

customShellApp :: Text -> ShellApp ChargeStatus
customShellApp cmd =
  MkShellApp
    { command = MkCommand cmd,
      -- Reuse UPower's parser
      parser = UPower.chargeStatusShellApp ^. #parser
    }
