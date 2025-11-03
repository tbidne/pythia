{-# LANGUAGE QuasiQuotes #-}

-- | This module provides functionality for retrieving network connection
-- information using ip utility.
--
-- @since 0.1
module Pythia.Services.NetInterface.Ip
  ( -- * Query
    netInterfaceShellApp,
    supported,

    -- * Misc
    IpParseError (..),
    parseInterfaces,
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Aeson (FromJSON (parseJSON), (.:))
import Data.Aeson qualified as Asn
import Data.Text qualified as T
import Pythia.Control.Exception (fromPythiaException, toPythiaException)
import Pythia.Internal.ShellApp
  ( SimpleShell
      ( MkSimpleShell,
        command,
        isSupported,
        parser
      ),
  )
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.NetInterface.Types
  ( NetInterface
      ( MkNetInterface,
        device,
        ipv4s,
        ipv6s,
        name,
        ntype,
        state
      ),
    NetInterfaceState (NetStateDown, NetStateUnknown, NetStateUp),
    NetInterfaces (MkNetInterfaces),
  )
import Pythia.Services.Types.Network
  ( IpAddress (MkIpAddress),
    IpAddresses (MkIpAddresses),
    IpType (Ipv4, Ipv6),
  )
import Pythia.Utils qualified as U
import Refined qualified as R

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

-- | Error parsing ip output.
--
-- ==== __Examples__
--
-- >>> displayException $ MkIpParseError "parse error"
-- "Ip parse error: parse error"
--
-- @since 0.1
type IpParseError :: Type
newtype IpParseError = MkIpParseError Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception IpParseError where
  displayException (MkIpParseError e) =
    ("Ip parse error: " <>)
      . T.unpack
      $ e

  toException = toPythiaException
  fromException = fromPythiaException

-- | Ip query for 'NetInterface'.
--
-- @since 0.1
netInterfaceShellApp ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  m NetInterfaces
netInterfaceShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "ip --json address",
          isSupported = supported,
          parser = parseInterfaces
        }
{-# INLINEABLE netInterfaceShellApp #-}

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: (HasCallStack, MonadPathReader m) => m Bool
supported = U.exeSupported [osp|ip|]
{-# INLINEABLE supported #-}

-- | Attempts to parse the output of IP.
--
-- @since 0.1
parseInterfaces :: Text -> Either IpParseError NetInterfaces
parseInterfaces txt = case Asn.eitherDecodeStrictText txt of
  Left ex -> Left $ MkIpParseError $ T.pack ex
  Right ifs -> Right $ MkNetInterfaces (unNetInterfaceIp <$> ifs)
{-# INLINEABLE parseInterfaces #-}

newtype NetInterfaceIp = MkNetInterfaceIp NetInterface

unNetInterfaceIp :: NetInterfaceIp -> NetInterface
unNetInterfaceIp (MkNetInterfaceIp ni) = ni

instance FromJSON NetInterfaceIp where
  parseJSON = Asn.withObject "NetInterfaceIp" $ \v -> do
    device <- v .: "ifname"

    rawAddresses <- v .: "addr_info"

    (ipv4s, ipv6s) <- case parseAddresses rawAddresses of
      Left _ -> undefined
      Right as -> pure as

    rawState <- v .: "operstate"
    let state = case rawState of
          "DOWN" -> NetStateDown
          "UP" -> NetStateUp
          other -> NetStateUnknown other

    pure
      $ MkNetInterfaceIp
      $ MkNetInterface
        { device,
          ipv4s,
          ipv6s,
          name = Nothing,
          ntype = Nothing,
          state
        }

newtype RawIpAddress = MkRawIpAddress (IpType, Text)

instance FromJSON RawIpAddress where
  parseJSON = Asn.withObject "RawIpAddress" $ \v -> do
    ty <- v .: "family"
    ipType <- case ty of
      "inet" -> pure Ipv4
      "inet6" -> pure Ipv6
      other -> fail $ "Unexepected ip type: " <> other

    ipTxt <- v .: "local"
    pure $ MkRawIpAddress (ipType, ipTxt)

parseAddresses :: [RawIpAddress] -> Either R.RefineException (IpAddresses Ipv4, IpAddresses Ipv6)
parseAddresses = fmap (bimap MkIpAddresses MkIpAddresses) . foldr go (Right ([], []))
  where
    go (MkRawIpAddress (ipType, rawTxt)) eAcc = case ipType of
      Ipv4 -> liftA2 addIpv4 (R.refine rawTxt) eAcc
      Ipv6 -> liftA2 addIpv6 (R.refine rawTxt) eAcc

    addIpv4 z (xs, ys) = (MkIpAddress z : xs, ys)
    addIpv6 z (xs, ys) = (xs, MkIpAddress z : ys)
