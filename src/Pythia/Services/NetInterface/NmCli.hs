{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving network connection
-- information using nmcli.
--
-- @since 0.1.0.0
module Pythia.Services.NetInterface.NmCli
  ( -- * Query
    netInterfaceShellApp,
    supported,

    -- * Misc
    NmCliException (..),
    parseInterfaces,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as T
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Prelude
import Pythia.Services.NetInterface.Types
  ( NetInterface (..),
    NetInterfaceState (..),
    NetInterfaceType (..),
    NetInterfaces (..),
  )
import Pythia.Services.Types (Device (..), Ipv4Address (..), Ipv6Address (..))
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R
import Text.Megaparsec (ErrorFancy (..), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Errors that can occur with nmcli.
--
-- @since 0.1.0.0
data NmCliException
  = -- | General exceptions.
    --
    -- @since 0.1.0.0
    forall e. Exception e => NmCliGeneralException e
  | -- | Parse exceptions.
    --
    -- @since 0.1.0.0
    NmCliParseException String

-- | @since 0.1.0.0
makePrismLabels ''NmCliException

-- | @since 0.1.0.0
deriving stock instance Show NmCliException

-- | @since 0.1.0.0
instance PrettyPrinter NmCliException where
  pretty (NmCliGeneralException e) = "Nmcli exception: <" <> displayException e <> ">"
  pretty (NmCliParseException s) = "Nmcli parse exception: <" <> show s <> ">"

-- | @since 0.1.0.0
instance Exception NmCliException where
  displayException = pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | NmCli query for 'NetInterfaces'. Throws exceptions if the command fails
-- or we have a parse error.
--
-- @since 0.1.0.0
netInterfaceShellApp :: (MonadCatch m, MonadIO m) => m NetInterfaces
netInterfaceShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "nmcli -t -m multiline device show",
          parser = parseInterfaces,
          liftShellEx = NmCliGeneralException
        }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1.0.0
supported :: MonadIO m => m Bool
supported = U.exeSupported "nmcli"

type MParser = Parsec Void Text

-- | Attemps to parse the output of nmcli.
--
-- @since 0.1.0.0
parseInterfaces :: Text -> Either NmCliException NetInterfaces
parseInterfaces txt = case MP.parse mparseInterfaces "Pythia.Services.NetInterface.NmCli" txt of
  Left ex ->
    let prettyErr = MP.errorBundlePretty ex
     in Left $ NmCliParseException prettyErr
  Right ifs -> Right ifs

mparseInterfaces :: MParser NetInterfaces
mparseInterfaces = MkNetInterfaces <$> MP.many parseInterface

parseInterface :: MParser NetInterface
parseInterface = do
  device' <- parseDevice
  type' <- parseNetInterfaceType
  parseHwaddr
  parseMTU
  state' <- parseNetInterfaceState
  name' <- parseName
  parseConPath
  MP.optional parseWiredProp
  ipv4s' <- U.mAlt <$> MP.optional parseIpv4s
  ipv6s' <- U.mAlt <$> MP.optional parseIpv6s
  MP.many MPC.eol
  pure $
    MkNetInterface
      { idevice = device',
        itype = Just type',
        istate = state',
        iname = name',
        ipv4s = ipv4s',
        ipv6s = ipv6s'
      }

parseDevice :: MParser Device
parseDevice = do
  MPC.string "GENERAL.DEVICE:"
  device' <- U.takeLineLabel (Just "device")
  pure $ MkDevice device'

parseNetInterfaceType :: MParser NetInterfaceType
parseNetInterfaceType = do
  MPC.string "GENERAL.TYPE:"
  type' <-
    MP.try wifiP2p
      <|> MP.try wifi
      <|> MP.try ethernet
      <|> MP.try loopback
      <|> MP.try tun
      <|> unknown
      <?> "type"
  MPC.eol
  pure type'
  where
    wifi = MPC.string "wifi" $> Wifi
    wifiP2p = MPC.string "wifi-p2p" $> Wifi_P2P
    ethernet = MPC.string "ethernet" $> Ethernet
    loopback = MPC.string "loopback" $> Loopback
    tun = MPC.string "tun" $> Tun
    unknown = UnknownType <$> MP.takeWhile1P Nothing (/= '\n')

parseHwaddr :: MParser ()
parseHwaddr = MPC.string "GENERAL.HWADDR:" *> U.takeLine_

parseMTU :: MParser ()
parseMTU = MPC.string "GENERAL.MTU:" *> U.takeLine_

parseNetInterfaceState :: MParser NetInterfaceState
parseNetInterfaceState = do
  MPC.string "GENERAL.STATE:"
  MP.takeWhile1P (Just "state int code") Char.isDigit
  MPC.space
  MPC.char '('
  state' <-
    MP.try up
      <|> MP.try down
      <|> MP.try unavail
      <|> unknown
      <?> "state"
  MPC.char ')'
  U.takeLine_
  pure state'
  where
    up = MPC.string "connected" $> Up
    down = MPC.string "disconnected" $> Down
    unavail = MPC.string "unavailable" $> Down
    unknown = UnknownState <$> MP.takeWhile1P (Just "type") (/= ')')

parseName :: MParser (Maybe Text)
parseName = do
  MPC.string "GENERAL.CONNECTION:"
  name' <- U.takeLineLabel (Just "name")
  pure $
    if T.null name'
      then Nothing
      else Just name'

parseConPath :: MParser ()
parseConPath = MPC.string "GENERAL.CON-PATH:" *> U.takeLine_

parseWiredProp :: MParser ()
parseWiredProp = MPC.string "WIRED-PROPERTIES" *> U.takeLine_

parseIpv4s :: MParser [Ipv4Address]
parseIpv4s = parseAllIpInfo "4" MkIpv4Address

parseIpv6s :: MParser [Ipv6Address]
parseIpv6s = parseAllIpInfo "6" MkIpv6Address

parseAllIpInfo :: Predicate p Text => Text -> (Refined p Text -> a) -> MParser [a]
parseAllIpInfo p cons = do
  ipvs <- parseIps p cons
  parseGateway p
  parseRoutes p
  parseDns p
  pure ipvs

parseIps :: Predicate p Text => Text -> (Refined p Text -> a) -> MParser [a]
parseIps p cons = do
  addrs <- parseAddresses p
  let xs = traverse R.refine addrs
  case xs of
    Left ex ->
      let errMsg :: String
          errMsg =
            "Malformed ipv"
              <> T.unpack p
              <> " address found: "
              <> show (T.unpack <$> addrs)
              <> ". Error: "
              <> show ex
       in MP.fancyFailure $ Set.fromList [ErrorFail errMsg]
    Right xss -> pure (cons <$> xss)

parseAddresses :: Text -> MParser [Text]
parseAddresses = MP.many . address
  where
    address p = do
      MPC.string $ "IP" <> p <> ".ADDRESS["
      MP.takeWhile1P (Just "address num") Char.isDigit
      MPC.string "]:"
      addr <- MP.takeWhile1P (Just "address") (/= '/')
      MPC.char '/'
      U.takeLine
      pure addr

parseGateway :: Text -> MParser ()
parseGateway p = MPC.string ("IP" <> p <> ".GATEWAY:") *> U.takeLine_

parseRoutes :: Text -> MParser ()
parseRoutes = void . MP.many . route
  where
    route p =
      MPC.string ("IP" <> p <> ".ROUTE[") *> U.takeLine_

parseDns :: Text -> MParser ()
parseDns = void . MP.many . dns
  where
    dns p = MPC.string ("IP" <> p <> ".DNS[") *> U.takeLine_
