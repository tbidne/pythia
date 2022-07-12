{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving network connection
-- information using nmcli.
--
-- @since 0.1
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
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Internal.ShellApp (SimpleShell (..))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.NetInterface.Types
  ( NetInterface (..),
    NetInterfaceState (..),
    NetInterfaceType (..),
    NetInterfaces (..),
  )
import Pythia.Services.Types.Network
  ( Device (..),
    IpAddress (..),
    IpAddresses (..),
    IpType (..),
  )
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R
import Text.Megaparsec (ErrorFancy (..), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> import GHC.Exception (errorCallException)

-- | Errors that can occur with nmcli.
--
-- ==== __Examples__
--
-- >>> putStrLn $ displayException $ NmCliGeneralException $ errorCallException "oh no"
-- NmCli exception: <oh no>
--
-- >>> putStrLn $ displayException $ NmCliParseException "parse error"
-- NmCli parse exception: <parse error>
--
-- @since 0.1
type NmCliException :: Type
data NmCliException
  = -- | General exceptions.
    --
    -- @since 0.1
    forall e. Exception e => NmCliGeneralException e
  | -- | Parse exceptions.
    --
    -- @since 0.1
    NmCliParseException Text

-- | @since 0.1
makePrisms ''NmCliException

-- | @since 0.1
deriving stock instance Show NmCliException

-- | @since 0.1
instance Pretty NmCliException where
  pretty (NmCliGeneralException e) =
    pretty @Text "NmCli exception: <"
      <> pretty (displayException e)
      <> pretty @Text ">"
  pretty (NmCliParseException s) =
    pretty @Text "NmCli parse exception: <"
      <> pretty s
      <> pretty @Text ">"
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception NmCliException where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | NmCli query for 'NetInterfaces'.
--
-- __Throws:__
--
-- * 'NmCliException': if something goes wrong (i.e. exception while running
--       the command, or we have a parse error).
--
-- @since 0.1
netInterfaceShellApp :: IO NetInterfaces
netInterfaceShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "nmcli -t -m multiline device show",
          parser = parseInterfaces,
          liftShellEx = NmCliGeneralException
        }
{-# INLINEABLE netInterfaceShellApp #-}

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: IO Bool
supported = U.exeSupported "nmcli"
{-# INLINEABLE supported #-}

type MParser :: Type -> Type
type MParser = Parsec Void Text

-- | Attemps to parse the output of nmcli.
--
-- @since 0.1
parseInterfaces :: Text -> Either NmCliException NetInterfaces
parseInterfaces txt = case MP.parse mparseInterfaces "Pythia.Services.NetInterface.NmCli" txt of
  Left ex ->
    let prettyErr = MP.errorBundlePretty ex
     in Left $ NmCliParseException $ T.pack prettyErr
  Right ifs -> Right ifs
{-# INLINEABLE parseInterfaces #-}

mparseInterfaces :: MParser NetInterfaces
mparseInterfaces = MkNetInterfaces <$> MP.many parseInterface
{-# INLINEABLE mparseInterfaces #-}

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
      { device = device',
        ntype = Just type',
        state = state',
        name = name',
        ipv4s = MkIpAddresses ipv4s',
        ipv6s = MkIpAddresses ipv6s'
      }
{-# INLINEABLE parseInterface #-}

parseDevice :: MParser Device
parseDevice = do
  MPC.string "GENERAL.DEVICE:"
  device' <- U.takeLineLabel (Just "device")
  pure $ MkDevice device'
{-# INLINEABLE parseDevice #-}

parseNetInterfaceType :: MParser NetInterfaceType
parseNetInterfaceType = do
  MPC.string "GENERAL.TYPE:"
  type' <-
    MP.try wifiP2p
      <|> MP.try wifi
      <|> MP.try ethernet
      <|> MP.try loopback
      <|> MP.try tun
      <|> MP.fancyFailure (Set.fromList [ErrorFail "Unknown type"])
      <?> "type"
  MPC.eol
  pure type'
  where
    wifi = MPC.string "wifi" $> Wifi
    wifiP2p = MPC.string "wifi-p2p" $> Wifi_P2P
    ethernet = MPC.string "ethernet" $> Ethernet
    loopback = MPC.string "loopback" $> Loopback
    tun = MPC.string "tun" $> Tun
{-# INLINEABLE parseNetInterfaceType #-}

parseHwaddr :: MParser ()
parseHwaddr = MPC.string "GENERAL.HWADDR:" *> U.takeLine_
{-# INLINEABLE parseHwaddr #-}

parseMTU :: MParser ()
parseMTU = MPC.string "GENERAL.MTU:" *> U.takeLine_
{-# INLINEABLE parseMTU #-}

parseNetInterfaceState :: MParser NetInterfaceState
parseNetInterfaceState = do
  MPC.string "GENERAL.STATE:"
  MP.takeWhile1P (Just "state int code") Char.isDigit
  MPC.space
  state' <-
    MP.try up
      <|> MP.try down
      <|> MP.try unavail
      <|> unknown
      <?> "state"
  U.takeLine_
  pure state'
  where
    up = MPC.string "(connected)" $> Up
    down = MPC.string "(disconnected)" $> Down
    unavail = MPC.string "(unavailable)" $> Down
    unknown = UnknownState <$> MP.takeWhile1P (Just "type") (/= '\n')
{-# INLINEABLE parseNetInterfaceState #-}

parseName :: MParser (Maybe Text)
parseName = do
  MPC.string "GENERAL.CONNECTION:"
  name' <- U.takeLineLabel (Just "name")
  pure $
    if T.null name'
      then Nothing
      else Just name'
{-# INLINEABLE parseName #-}

parseConPath :: MParser ()
parseConPath = MPC.string "GENERAL.CON-PATH:" *> U.takeLine_
{-# INLINEABLE parseConPath #-}

parseWiredProp :: MParser ()
parseWiredProp = MPC.string "WIRED-PROPERTIES" *> U.takeLine_
{-# INLINEABLE parseWiredProp #-}

parseIpv4s :: MParser [IpAddress 'Ipv4]
parseIpv4s = parseAllIpInfo "4" MkIpAddress
{-# INLINEABLE parseIpv4s #-}

parseIpv6s :: MParser [IpAddress 'Ipv6]
parseIpv6s = parseAllIpInfo "6" MkIpAddress
{-# INLINEABLE parseIpv6s #-}

parseAllIpInfo :: Predicate p Text => Text -> (Refined p Text -> a) -> MParser [a]
parseAllIpInfo p cons = do
  ipvs <- parseIps p cons
  parseGateway p
  parseRoutes p
  parseDns p
  parseDomain p
  pure ipvs
{-# INLINEABLE parseAllIpInfo #-}

parseIps :: Predicate p Text => Text -> (Refined p Text -> a) -> MParser [a]
parseIps p cons = do
  addrs <- parseAddresses p
  let xs = traverse R.refine addrs
  case xs of
    Left ex ->
      let errMsg :: String
          errMsg =
            T.unpack $
              "Malformed ipv"
                <> p
                <> " address found: "
                <> showt addrs
                <> ". Error: "
                <> showt ex
       in MP.fancyFailure $ Set.fromList [ErrorFail errMsg]
    Right xss -> pure (cons <$> xss)
{-# INLINEABLE parseIps #-}

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
{-# INLINEABLE parseAddresses #-}

parseGateway :: Text -> MParser ()
parseGateway p = MPC.string ("IP" <> p <> ".GATEWAY:") *> U.takeLine_
{-# INLINEABLE parseGateway #-}

parseRoutes :: Text -> MParser ()
parseRoutes = void . MP.many . route
  where
    route p =
      MPC.string ("IP" <> p <> ".ROUTE[") *> U.takeLine_
{-# INLINEABLE parseRoutes #-}

parseDns :: Text -> MParser ()
parseDns = void . MP.many . dns
  where
    dns p = MPC.string ("IP" <> p <> ".DNS[") *> U.takeLine_
{-# INLINEABLE parseDns #-}

parseDomain :: Text -> MParser ()
parseDomain = void . MP.many . dns
  where
    dns p = MPC.string ("IP" <> p <> ".DOMAIN[") *> U.takeLine_
{-# INLINEABLE parseDomain #-}
