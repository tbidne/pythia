-- | This module provides functionality for retrieving battery information
-- using UPower.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.SysFs
  ( batteryShellApp,
  )
where

import Control.Monad.Trans.Except qualified as ExceptT
import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Data (QueryError (..))
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryLevel,
    BatteryState (..),
  )
import Pythia.ShellApp (GeneralShell (..), ShellApp (..))
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Text.Read qualified as TR

-- | /sys/class 'ShellApp' for 'Battery'.
--
-- @since 0.1.0.0
batteryShellApp :: ShellApp Battery
batteryShellApp =
  GeneralApp $
    MkGeneralShell $ ExceptT.runExceptT queryBattery

type Result a = ExceptT [QueryError] IO a

queryBattery :: Result Battery
queryBattery = do
  sysExists <- liftIO $ Dir.doesDirectoryExist sys
  sysBase <-
    if sysExists
      then pure sys
      else do
        sysFsExists <- liftIO $ Dir.doesDirectoryExist sysFs
        if sysFsExists
          then pure sysFs
          else ExceptT.throwE [noSysErr]

  batDir <- ExceptT (findBatteryDir sysBase)
  statePath <- dirExistsExceptT (batDir </> "status")
  state <- parseState statePath
  percentPath <- dirExistsExceptT (batDir </> "capacity")
  level <- parseLevel percentPath
  pure $ MkBattery level state
  where
    sys = "/sys/class/power_supply"
    sysFs = "/sysFs/class/power_supply"
    noSysErr =
      MkQueryError
        { name = "Pythia.Services.Battery.SysFs",
          short = "/sys error",
          long = "No " <> T.pack sys <> " or " <> T.pack sysFs <> " directory found"
        }

findBatteryDir :: FilePath -> IO (Either [QueryError] FilePath)
findBatteryDir sysBase = foldr firstExists (pure (Left [noneFoundErr])) batDirs
  where
    firstExists bd acc = do
      e <- dirExists (sysBase </> bd)
      case e of
        Nothing -> acc
        Just e' -> pure $ Right e'
    batDirs =
      [ "BAT0",
        "BAT1",
        "BAT2",
        "BAT3",
        "BAT4",
        "BAT5",
        "BAT"
      ]
    noneFoundErr =
      MkQueryError
        { name = "Pythia.Services.Battery.SysFs",
          short = "Exe error",
          long = "No BAT[0-5]* directory found"
        }

dirExists :: FilePath -> IO (Maybe FilePath)
dirExists fp = do
  b <- Dir.doesDirectoryExist fp
  pure $
    if b
      then Just fp
      else Nothing

dirExistsExceptT :: FilePath -> Result FilePath
dirExistsExceptT fp = do
  b <- liftIO $ Dir.doesFileExist fp
  if b
    then pure fp
    else throwE [err]
  where
    err =
      MkQueryError
        { name = "Pythia.Services.Battery.SysFs",
          short = "FileNotExist",
          long = "Could not find file: " <> T.pack fp
        }

parseState :: FilePath -> Result BatteryState
parseState fp = do
  stateTxt <- T.toLower . T.strip <$> readFileUtf8LenientExceptT mkFileErr fp
  case stateTxt of
    "charging" -> pure Charging
    "discharging" -> pure Discharging
    "not charging" -> pure Pending
    "full" -> pure Full
    bad -> pure $ Unknown bad
  where
    mkFileErr ex =
      [ MkQueryError
          { name = "Pythia.Services.Battery.SysFs",
            short = "FileNotFound",
            long =
              "Could not find battery state file at "
                <> T.pack (show fp)
                <> ": "
                <> T.pack (show ex)
          }
      ]

parseLevel :: FilePath -> Result BatteryLevel
parseLevel fp = do
  percentTxt <- readFileUtf8LenientExceptT mkFileErr fp
  case readInterval percentTxt of
    Nothing -> throwE undefined
    Just bs -> pure bs
  where
    mkFileErr ex =
      [ MkQueryError
          { name = "Pythia.Services.Battery.SysFs",
            short = "FileNotFound",
            long =
              "Could not find battery state file at "
                <> T.pack (show fp)
                <> ": "
                <> T.pack (show ex)
          }
      ]
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack
