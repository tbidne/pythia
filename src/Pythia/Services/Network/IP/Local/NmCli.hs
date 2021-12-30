-- | This module provides functionality for retrieving ip address
-- information using nmcli.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Local.NmCli
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
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core ((^.))
import Pythia.Data (QueryError (..))
import Pythia.Data qualified as E
import Pythia.Services.Network.IP.Local.Types
  ( Ipv4 (..),
    Ipv6 (..),
    LocalIpAddresses (..),
    LocalIps (..),
  )
import Pythia.Services.Network.Types (Device (..))
import Pythia.ShellApp (ShellApp (..), SimpleShell (..))
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R

-- | NmCli 'ShellApp' for 'LocalIps'.
--
-- @since 0.1.0.0
ipShellApp :: Device -> ShellApp LocalIps
ipShellApp deviceName =
  SimpleApp $
    MkSimpleShell
      { command = "nmcli -t device show",
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
        { E.name = "Pythia.Services.Network.LocalIps.NmCli",
          E.short = s,
          E.long = l
        }

parseIP :: Parser LocalIps
parseIP = do
  device <- parseDevice
  parsePreamble
  end <- AP.atEnd
  let mkIP = MkLocalIps device
  if end
    then pure $ mkIP mempty
    else do
      mAddresses <- A.optional parseAllAddresses
      pure $ maybe (mkIP mempty) mkIP mAddresses

parseAllAddresses :: Parser LocalIpAddresses
parseAllAddresses = do
  ipv4s <- A.many parseIPv4
  parseOtherNetwork "IP4"
  ipv6s <- A.many parseIPv6
  parseOtherNetwork "IP6"
  pure $ MkLocalIpAddresses ipv4s ipv6s

parseDevice :: Parser Device
parseDevice =
  MkDevice
    <$> ( AP.string "GENERAL.DEVICE:"
            *> AP.takeWhile1 (not . AP.isEndOfLine)
            <* AP.endOfLine
        )

parseIPv4 :: Parser Ipv4
parseIPv4 =
  MkIpv4
    <$> ( AP.string "IP4.ADDRESS["
            *> AP.digit
            *> AP.string "]:"
            *> (parseIPStr >>= stringsToIP)
            <* AP.char '/'
            <* AP.scientific
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
    <$> ( AP.string "IP6.ADDRESS["
            *> AP.digit
            *> AP.string "]:"
            *> (parseIPStr >>= stringsToIP)
            <* AP.char '/'
            <* AP.scientific
            <* U.takeLine1
        )
  where
    parseIPStr =
      AP.many1 (parseHex <|> parseColon)
    parseHex = AP.takeWhile1 C.isHexDigit
    parseColon = AP.string ":"

stringsToIP :: (Predicate p a, Monoid a) => [a] -> Parser (Refined p a)
stringsToIP strs = case R.refine (mconcat strs) of
  Left _ -> mempty
  Right re -> pure re

parsePreamble :: Parser ()
parsePreamble = do
  AP.string "GENERAL.TYPE:" *> U.takeLine1
  AP.string "GENERAL.HWADDR:" *> U.takeLine1
  AP.string "GENERAL.MTU:" *> U.takeLine1
  AP.string "GENERAL.STATE:" *> U.takeLine1
  AP.string "GENERAL.CONNECTION:" *> U.takeLine1
  AP.string "GENERAL.CON-PATH:" *> U.takeLine1
  AP.option () (AP.string "WIRED-PROPERTIES" *> U.takeLine1)

parseOtherNetwork :: Text -> Parser ()
parseOtherNetwork prefix =
  AP.string (prefix <> ".GATEWAY:") *> U.takeLine1
    *> parseRoute prefix
    *> parseDNS prefix
    *> parseDomain prefix

parseRoute :: Text -> Parser ()
parseRoute prefix = parseMultiple (prefix <> ".ROUTE")

parseDNS :: Text -> Parser ()
parseDNS prefix = parseMultiple (prefix <> ".DNS")

parseDomain :: Text -> Parser ()
parseDomain prefix = parseMultiple (prefix <> ".DOMAIN")

parseMultiple :: Text -> Parser ()
parseMultiple value =
  AP.many'
    ( AP.string value
        *> U.takeLine1
    )
    $> ()

--takeLine :: Parser ()
--takeLine =
--  AP.takeWhile (not . AP.isEndOfLine)
--   *> AP.skipSpace
