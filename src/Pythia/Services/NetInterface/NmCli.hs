{-# LANGUAGE QuasiQuotes #-}

-- | This module provides functionality for retrieving network connection
-- information using nmcli.
--
-- @since 0.1
module Pythia.Services.NetInterface.NmCli
  ( -- * Query
    netInterfaceShellApp,
    supported,

    -- * Misc
    NmCliParseError (..),
    parseInterfaces,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as T
import Pythia.Internal.ShellApp (SimpleShell (MkSimpleShell))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.NetInterface.Types
  ( NetInterface (MkNetInterface, device, ipv4s, ipv6s, name, ntype, state),
    NetInterfaceState (NetStateDown, NetStateUnknown, NetStateUp),
    NetInterfaceType (Bridge, Ethernet, Loopback, Tun, Unknown, Wifi, Wifi_P2P),
    NetInterfaces (MkNetInterfaces),
  )
import Pythia.Services.Types.Network
  ( Device (MkDevice),
    IpAddress (MkIpAddress),
    IpAddresses (MkIpAddresses),
    IpType (Ipv4, Ipv6),
  )
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R
import Text.Megaparsec (ErrorFancy (ErrorFail), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

-- | Error parsing nmcli output.
--
-- ==== __Examples__
--
-- >>> displayException $ MkNmCliParseError "parse error"
-- "NmCli parse error: parse error"
--
-- @since 0.1
type NmCliParseError :: Type
newtype NmCliParseError = MkNmCliParseError Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception NmCliParseError where
  displayException (MkNmCliParseError e) =
    ("NmCli parse error: " <>)
      . T.unpack
      $ e

-- | NmCli query for 'NetInterfaces'.
--
-- @since 0.1
netInterfaceShellApp ::
  ( MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  m NetInterfaces
netInterfaceShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "nmcli -t -m multiline device show",
          isSupported = supported,
          parser = parseInterfaces
        }
{-# INLINEABLE netInterfaceShellApp #-}

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: (MonadPathReader m) => m Bool
supported = U.exeSupported [osp|nmcli|]
{-# INLINEABLE supported #-}

type MParser :: Type -> Type
type MParser = Parsec Void Text

-- | Attemps to parse the output of nmcli.
--
-- @since 0.1
parseInterfaces :: Text -> Either NmCliParseError NetInterfaces
parseInterfaces txt = case MP.parse mparseInterfaces "Pythia.Services.NetInterface.NmCli" txt of
  Left ex ->
    let prettyErr = MP.errorBundlePretty ex
     in Left $ MkNmCliParseError $ T.pack prettyErr
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
  pure
    $ MkNetInterface
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
      <|> MP.try bridge
      <|> MP.try tun
      <|> MP.try unknown
      <|> MP.fancyFailure (Set.fromList [ErrorFail "Unknown type"])
      <?> "type"
  MPC.eol
  pure type'
  where
    bridge = MPC.string "bridge" $> Bridge
    ethernet = MPC.string "ethernet" $> Ethernet
    loopback = MPC.string "loopback" $> Loopback
    tun = MPC.string "tun" $> Tun
    unknown = Unknown <$> MP.takeWhileP (Just "type") (/= '\n')
    wifi = MPC.string "wifi" $> Wifi
    wifiP2p = MPC.string "wifi-p2p" $> Wifi_P2P
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
    up = MPC.string "(connected)" $> NetStateUp
    down = MPC.string "(disconnected)" $> NetStateDown
    unavail = MPC.string "(unavailable)" $> NetStateDown
    unknown = NetStateUnknown <$> MP.takeWhile1P (Just "type") (/= '\n')
{-# INLINEABLE parseNetInterfaceState #-}

parseName :: MParser (Maybe Text)
parseName = do
  MPC.string "GENERAL.CONNECTION:"
  name' <- U.takeLineLabel (Just "name")
  pure
    $ if T.null name'
      then Nothing
      else Just name'
{-# INLINEABLE parseName #-}

parseConPath :: MParser ()
parseConPath = MPC.string "GENERAL.CON-PATH:" *> U.takeLine_
{-# INLINEABLE parseConPath #-}

parseWiredProp :: MParser ()
parseWiredProp = MPC.string "WIRED-PROPERTIES" *> U.takeLine_
{-# INLINEABLE parseWiredProp #-}

parseIpv4s :: MParser [IpAddress Ipv4]
parseIpv4s = parseAllIpInfo "4" MkIpAddress
{-# INLINEABLE parseIpv4s #-}

parseIpv6s :: MParser [IpAddress Ipv6]
parseIpv6s = parseAllIpInfo "6" MkIpAddress
{-# INLINEABLE parseIpv6s #-}

parseAllIpInfo :: (Predicate p Text) => Text -> (Refined p Text -> a) -> MParser [a]
parseAllIpInfo p cons = do
  ipvs <- parseIps p cons
  parseGateway p
  parseRoutes p
  parseDns p
  parseDomain p
  pure ipvs
{-# INLINEABLE parseAllIpInfo #-}

parseIps :: (Predicate p Text) => Text -> (Refined p Text -> a) -> MParser [a]
parseIps p cons = do
  addrs <- parseAddresses p
  let xs = traverse R.refine addrs
  case xs of
    Left ex ->
      let errMsg :: String
          errMsg =
            T.unpack
              $ "Malformed ipv"
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
