{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the functionality for running shell
-- commands and parsing the result.
--
-- @since 0.1.0.0
module Pythia.ShellApp
  ( -- * Types
    ShellApp (..),
    SimpleShell (..),
    GeneralShell (..),
    QueryResult,

    -- * Running a 'ShellApp'
    runShellApp,
    tryApps,

    -- * Utilities
    runCommand,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import GHC.IO.Exception (ExitCode (..))
import Pythia.Data (Command (..), QueryError (..), QueryResult)
import Pythia.Prelude
import System.Process.Typed qualified as TP

-- | Type for running a "simple" shell command given by 'Command'.
-- The 'parser' is used to parse the result.
--
-- @since 0.1.0.0
data SimpleShell result = MkSimpleShell
  { -- | The shell command to run.
    --
    -- @since 0.1.0.0
    command :: Command,
    -- | The parser for the result of running the command.
    --
    -- @since 0.1.0.0
    parser :: Text -> Either QueryError result
  }

makeFieldLabelsNoPrefix ''SimpleShell

-- | Type for running a more general action than 'SimpleShell'. As this is
-- 'IO' we have more freedom to implement more complex behavior, e.g.,
-- running multiple commands, non-deterministic behavior, etc.
--
-- @since 0.1.0.0
newtype GeneralShell result = MkGeneralShell
  { -- | The action to run.
    --
    -- @since 0.1.0.0
    query :: IO (QueryResult result)
  }

makeFieldLabelsNoPrefix ''GeneralShell

-- | Sum type for running shell applications. Most actions should be simple
-- shell command + parse output, hence 'SimpleShell', but we also provide
-- 'GeneralShell' for when more complex behavior is needed.
--
-- @since 0.1.0.0
data ShellApp result
  = -- | @since 0.1.0.0
    SimpleApp (SimpleShell result)
  | -- | @since 0.1.0.0
    GeneralApp (GeneralShell result)

makePrismLabels ''ShellApp

-- | Runs the shell app and returns either the result or any errors
-- encountered.
--
-- @since 0.1.0.0
runShellApp :: ShellApp result -> IO (QueryResult result)
runShellApp (SimpleApp simple) = (_Left %~ (: [])) <$> runSimple simple
runShellApp (GeneralApp general) = general ^. #query

runSimple :: SimpleShell result -> IO (Either QueryError result)
runSimple simple =
  runCommand (simple ^. #command)
    >>= \t -> pure $ t >>= (simple ^. #parser)

-- | Runs a 'Command' and returns either the text result or error encountered.
-- This is used by 'SimpleShell' to run its command before the result is
-- parsed. This function is exported as it can be convenient to use with a
-- 'GeneralShell' (e.g. running multiple commands via 'runCommand').
--
-- @since 0.1.0.0
runCommand :: Command -> IO (Either QueryError Text)
runCommand command = do
  (exitCode, out, err) <- TP.readProcess $ TP.shell $ T.unpack cmdStr
  pure $ case exitCode of
    ExitSuccess -> case TEnc.decodeUtf8' (LBS.toStrict out) of
      Right result -> Right result
      Left ex -> Left $ utf8Err $ T.pack (show ex)
    ExitFailure n ->
      Left $ shellErr n cmdStr (LBS.toStrict err)
  where
    cmdStr = command ^. #unCommand

utf8Err :: Text -> QueryError
utf8Err err =
  MkQueryError
    { name = "Pythia.ShellApp",
      short = "Decode UTF-8 error",
      long = err
    }

shellErr :: Int -> Text -> ByteString -> QueryError
shellErr exitCode cmd err =
  MkQueryError
    { name = "Pythia.ShellApp",
      short = "Shell error",
      long = long
    }
  where
    err' = case TEnc.decodeUtf8' err of
      Right result -> result
      Left ex -> "<decode utf 8 err>: " <> T.pack (show ex)
    long =
      T.concat
        [ "Error running command `",
          cmd,
          "`. Received exit code ",
          T.pack $ show exitCode,
          " and message: ",
          err'
        ]

-- | Queries for information via multiple apps. Returns the first success
-- or all errors, if there are no successes.
--
-- @since 0.1.0.0
tryApps ::
  Show a =>
  (a -> ShellApp r) ->
  [(a, IO Bool)] ->
  IO (QueryResult r)
tryApps queryApp = foldr (tryApp queryApp) (pure (Left []))

tryApp ::
  Show a =>
  (a -> ShellApp r) ->
  (a, IO Bool) ->
  IO (QueryResult r) ->
  IO (QueryResult r)
tryApp toShellApp (app, supportedFn) acc = do
  isSupported <- supportedFn
  if isSupported
    then do
      eResult <- runShellApp (toShellApp app)
      case eResult of
        Right result -> pure $ Right result
        Left errs -> appendErrs errs <$> acc
    else appendErr (notSupported app) <$> acc
  where
    notSupported app' =
      MkQueryError
        { name = "Pythia.ShellApp",
          short = "AppError",
          long = "App not supported: " <> T.pack (show app')
        }

    appendErr :: a -> Either [a] b -> Either [a] b
    appendErr e = _Left %~ (e :)

    appendErrs :: [a] -> Either [a] b -> Either [a] b
    appendErrs errs = _Left %~ (<> errs)
