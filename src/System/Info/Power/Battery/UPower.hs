-- | This module provides functionality for retrieving battery information
-- using UPower.
module System.Info.Power.Battery.UPower
  ( P.parseBattery,
    command,
  )
where

import System.Info.Data.Command (Command)
import System.Info.Power.Battery.UPower.Parsing qualified as P

-- | This is the shell command that utilizes UPower to retrieve
-- battery information.
command :: Command
command = "upower -i `upower -e | grep 'BAT'`"
