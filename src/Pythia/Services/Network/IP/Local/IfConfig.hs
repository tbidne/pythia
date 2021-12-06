-- | This module provides functionality for retrieving ip address
-- information using ifconfig.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Local.IfConfig
  ( ipShellApp,
  )
where

import Control.Applicative (Alternative (..))
import Control.Applicative qualified as A
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Bifunctor (Bifunctor (..))
import Data.Char qualified as C
import Data.Data (Proxy)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core ((^.))
import Pythia.Data (QueryError (..))
import Pythia.Data qualified as E
import Pythia.Services.Network.IP.Local.Types
  ( Ipv4 (..),
    Ipv6 (..),
    LocalIpAddresses (MkLocalIpAddresses),
    LocalIps (..),
  )
import Pythia.Services.Network.Types (Device (..))
import Pythia.ShellApp (ShellApp (..), SimpleShell (..))
import Pythia.Utils qualified as U
import Refined (Refined)
import Refined qualified as R

-- | Ifconfig 'ShellApp' for 'LocalIps'.
--
-- @since 0.1.0.0
ipShellApp :: Device -> ShellApp LocalIps
ipShellApp deviceName =
  SimpleApp $
    MkSimpleShell
      { command = "ifconfig",
        parser = parseIPs deviceName
      }

parseIPs :: Device -> Text -> Either QueryError LocalIps
parseIPs device txt =
  let parseResult = AP.parseOnly (A.many parseIP) txt
   in first parseErr parseResult >>= findMatchingDevice
  where
    findMatchingDevice = foldr matchDevice devNotFoundErr
    matchDevice ip acc
      | ip ^. #localDevice == device = Right ip
      | otherwise = acc
    devNotFoundErr = Left $ mkErr "Parse error" ("Device not found: " <> device ^. #unDevice)
    parseErr = mkErr "Parse error" . T.pack
    mkErr s l =
      MkQueryError
        { E.name = "Pythia.Services.Network.LocalIps.IfConfig",
          E.short = s,
          E.long = l
        }

parseIP :: Parser LocalIps
parseIP = do
  device <- parseDevice
  addresses <- parseAllAddresses
  parseEpilogue
  pure $ MkLocalIps device addresses

parseAllAddresses :: Parser LocalIpAddresses
parseAllAddresses = do
  ipv4s <- A.many parseIPv4
  ipv6s <- A.many parseIPv6
  pure $ MkLocalIpAddresses ipv4s ipv6s

parseDevice :: Parser Device
parseDevice =
  MkDevice
    <$> ( AP.takeWhile1 (/= ':')
            <* U.takeLine1
        )

parseIPv4 :: Parser Ipv4
parseIPv4 =
  MkIpv4
    <$> ( AP.skipSpace
            *> AP.string "inet "
            *> (parseIPStr >>= stringsToIP)
            <* U.takeLine1
        )
  where
    parseIPStr =
      AP.many1 (parseDigit <|> parseDot)
    parseDigit = AP.takeWhile1 C.isDigit
    parseDot = AP.string "."

parseIPv6 :: Parser Ipv6
parseIPv6 =
  MkIpv6
    <$> ( AP.skipSpace
            *> AP.string "inet6 "
            *> (parseIPStr >>= stringsToIP)
            <* U.takeLine1
        )
  where
    parseIPStr =
      AP.many1 (parseHex <|> parseColon)
    parseHex = AP.takeWhile1 C.isHexDigit
    parseColon = AP.string ":"

stringsToIP :: (R.Predicate (Proxy ps) a, Monoid a) => [a] -> Parser (Refined ps a)
stringsToIP strs = case R.refineAll (mconcat strs) of
  Left _ -> mempty
  Right re -> pure re

parseEpilogue :: Parser ()
parseEpilogue =
  AP.many1
    ( AP.takeWhile1 AP.isHorizontalSpace
        *> U.takeLine1
    )
    $> ()
