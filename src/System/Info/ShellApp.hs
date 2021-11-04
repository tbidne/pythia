-- | This module provides the functionality for running a shell
-- command and parsing the result.
module System.Info.ShellApp
  ( ShellApp (..),
    runShellApp,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (UTF8 (..))
import Data.Text.Conversions qualified as Conv
import GHC.IO.Exception (ExitCode (..))
import Optics.Core (A_Lens, LabelOptic (..), (%), (^.))
import Optics.Core qualified as O
import System.Info.Data (Command (..), QueryError (..))
import System.Process.Typed qualified as TP

-- | Type for running a shell command and parsing the result.
data ShellApp result = MkShellApp
  { -- | The command to run.
    command :: Command,
    -- | The parser.
    parser :: Text -> Either QueryError result
  }

instance
  LabelOptic
    "command"
    A_Lens
    (ShellApp result)
    (ShellApp result)
    Command
    Command
  where
  labelOptic = O.lens command (\sa command' -> sa {command = command'})

instance Show (ShellApp result) where
  show sa = "MkShellApp {command = " <> cmd <> ", parser = <func>}"
    where
      cmd = show $ sa ^. #command

instance
  LabelOptic
    "parser"
    A_Lens
    (ShellApp result)
    (ShellApp result)
    (Text -> Either QueryError result)
    (Text -> Either QueryError result)
  where
  labelOptic = O.lens parser (\sa parser' -> sa {parser = parser'})

-- | Runs the given command and attempts to parse the result using the given
-- parser.
runShellApp ::
  ShellApp result ->
  IO (Either QueryError result)
runShellApp shellApp = do
  (exitCode, out, err) <- TP.readProcess $ TP.shell $ T.unpack cmdStr
  pure $ case exitCode of
    ExitSuccess -> case decodeUtf8 out of
      Just result -> shellApp ^. #parser $ result
      Nothing -> Left $ utf8Err $ T.pack (show out)
    ExitFailure n ->
      Left $ shellErr n cmdStr err
  where
    cmdStr = shellApp ^. #command % #unCommand

decodeUtf8 :: ByteString -> Maybe Text
decodeUtf8 = Conv.decodeConvertText . UTF8

utf8Err :: Text -> QueryError
utf8Err err =
  MkQueryError
    { name = "System.Info.ShellApp",
      short = "Decode UTF-8 error",
      long = err
    }

shellErr :: Int -> Text -> ByteString -> QueryError
shellErr exitCode cmd err =
  MkQueryError
    { name = "System.Info.ShellApp",
      short = "Shell error",
      long = long
    }
  where
    err' = M.fromMaybe "<decode utf8 err>" (decodeUtf8 err)
    long =
      T.concat
        [ "Error running command `",
          cmd,
          "`. Received exit code ",
          T.pack $ show exitCode,
          " and message: ",
          err'
        ]
