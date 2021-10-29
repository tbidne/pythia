-- | This module provides common utilities.
module System.Info.Utils
  ( runShellAndParse,
    headMaybe,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (UTF8 (..))
import Data.Text.Conversions qualified as Conv
import GHC.IO.Exception (ExitCode (..))
import System.Info.Data.Command (Command (..))
import System.Info.Data.Error (Error (..))
import System.Process.Typed qualified as TP

-- | Runs the given command and attempts to parse the result using the given
-- parser.
runShellAndParse ::
  (Text -> Either Error result) ->
  Command ->
  IO (Either Error result)
runShellAndParse parser (MkCommand cmd) = do
  (exitCode, out, err) <- TP.readProcess $ TP.shell $ T.unpack cmd
  pure $ case exitCode of
    ExitSuccess -> case decodeUtf8 out of
      Just result -> parser result
      Nothing -> Left $ utf8Err $ T.pack (show out)
    ExitFailure n ->
      Left $ shellErr n cmd err

-- | Safe version of 'head'.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

decodeUtf8 :: ByteString -> Maybe Text
decodeUtf8 = Conv.decodeConvertText . UTF8

utf8Err :: Text -> Error
utf8Err err =
  MkError
    { name = "System.Info.Utils",
      short = "Decode UTF-8 error",
      long = err
    }

shellErr :: Int -> Text -> ByteString -> Error
shellErr exitCode cmd err =
  MkError
    { name = "System.Info.Utils",
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
