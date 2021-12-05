-- | This module provides functionality for retrieving network connection
-- information using nmcli.
--
-- @since 0.1.0.0
module Pythia.Services.Network.Connection.NmCli
  ( connectionShellApp,
  )
where

import Control.Applicative (Alternative (..))
import Control.Applicative qualified as A
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core ((%), (^.))
import Optics.Fold qualified as O
import Optics.Getter qualified as O
import Pythia.Data (QueryError (..))
import Pythia.Data qualified as E
import Pythia.Services.Network.Connection.Types
  ( ConnState (..),
    ConnType (..),
    Connection (..),
  )
import Pythia.Services.Network.Types (Device (..))
import Pythia.ShellApp (ShellApp (..), SimpleShell (..))
import Pythia.Utils qualified as U

-- | NmCli 'ShellApp' for 'Connection'.
--
-- @since 0.1.0.0
connectionShellApp :: Device -> ShellApp Connection
connectionShellApp deviceName =
  SimpleApp $
    MkSimpleShell
      { command = "nmcli -m multiline device | cat",
        parser = parseConnection deviceName
      }

-- Attempts to parse the given text into a 'Connection'.
parseConnection :: Device -> Text -> Either QueryError Connection
parseConnection device txt = case AP.parseOnly (A.many connectionParser) txt of
  Right conns ->
    case findDevice conns of
      Just conn -> Right conn
      Nothing ->
        let devices = O.foldlOf' (O.folded % connDeviceName) combineDevices "" conns
         in Left $
              mkErr
                "Parse error"
                $ "Could not find device `"
                  <> device ^. #unDevice
                  <> "` in devices: "
                  <> devices
  Left err -> Left $ mkErr "Parse error" (T.pack err)
  where
    connDeviceName = #connDevice % #unDevice
    findDevice = U.headMaybe . filter ((== device ^. #unDevice) . O.view connDeviceName)
    combineDevices t "" = t
    combineDevices t acc = t <> ", " <> acc
    mkErr s l =
      MkQueryError
        { E.name = "Pythia.Services.Network.Connection.NmCli",
          E.short = s,
          E.long = l
        }

connectionParser :: Parser Connection
connectionParser =
  MkConnection
    <$> parseDevice
    <*> parseType
    <*> parseState
    <*> parseName

parseDevice :: Parser Device
parseDevice =
  MkDevice
    <$> ( AP.string "DEVICE:"
            *> AP.skipSpace
            *> AP.takeWhile1 (not . AP.isEndOfLine)
            <* AP.endOfLine
        )

parseType :: Parser ConnType
parseType =
  AP.string "TYPE:"
    *> AP.skipSpace
    *> parseAll
  where
    parseAll =
      AP.choice
        [ AP.string "wifi" <* AP.endOfLine $> Wifi,
          AP.string "wifi-p2p" <* AP.endOfLine $> Wifi_P2P,
          AP.string "ethernet" <* AP.endOfLine $> Ethernet,
          AP.string "loopback" <* AP.endOfLine $> Loopback,
          AP.string "tun" <* AP.endOfLine $> Tun,
          UnknownType <$> AP.takeWhile1 (not . AP.isEndOfLine) <* AP.endOfLine
        ]

parseState :: Parser ConnState
parseState =
  AP.string "STATE:"
    *> AP.skipSpace
    *> parseAll
  where
    parseAll =
      AP.choice
        [ AP.string "connected" <* AP.endOfLine $> Connected,
          AP.string "disconnected" <* AP.endOfLine $> Disconnected,
          AP.string "unavailable" <* AP.endOfLine $> Unavailable,
          AP.string "unmanaged" <* AP.endOfLine $> Unmanaged,
          UnknownState <$> AP.takeWhile1 (not . AP.isEndOfLine) <* AP.endOfLine
        ]

parseName :: Parser (Maybe Text)
parseName =
  AP.string "CONNECTION:"
    *> AP.skipSpace
    *> (parseHyphens <|> parseCName)
    <* AP.endOfLine
  where
    parseHyphens = AP.string "--" $> Nothing
    parseCName = Just <$> AP.takeWhile1 (not . AP.isEndOfLine)
