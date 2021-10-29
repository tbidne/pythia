-- | This module provides functionality for parsing network connection
-- information retrieved from the NetworkManager utility.
module System.Info.Network.Connection.NetworkManager.Parsing
  ( parseConnection,
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
import Optics.Core ((%))
import Optics.Fold qualified as O
import Optics.Getter qualified as O
import System.Info.Error (Error (MkError))
import System.Info.Error qualified as E
import System.Info.Network.Connection.Types (ConnState (..), ConnType (..), Connection (..))
import System.Info.Utils qualified as U

-- | Attempts to parse the given text into a 'Connection'.
parseConnection :: Text -> Text -> Either Error Connection
parseConnection deviceName txt = case AP.parseOnly (A.many connectionParser) txt of
  Right conns ->
    case findDevice conns of
      Just conn -> Right conn
      Nothing ->
        let devices = O.foldlOf' (O.folded % #device) combineDevices "" conns
         in Left $
              mkErr
                "Parse error"
                $ "Could not find device `"
                  <> deviceName
                  <> "` in devices: "
                  <> devices
  Left err -> Left $ mkErr "Parse error" (T.pack err)
  where
    findDevice = U.headMaybe . filter ((== deviceName) . O.view #device)
    combineDevices t "" = t
    combineDevices t acc = t <> ", " <> acc
    mkErr s l =
      MkError
        { E.name = "System.Info.Network.Connection.NetworkManager.Parsing",
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

parseDevice :: Parser Text
parseDevice =
  AP.string "DEVICE:"
    *> AP.skipSpace
    *> AP.takeWhile1 (not . AP.isEndOfLine)
    <* AP.endOfLine

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
