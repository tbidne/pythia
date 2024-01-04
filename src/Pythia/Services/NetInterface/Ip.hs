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

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as T
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

-- | Ip query for 'NetInterface'.
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
        { command = "ip address",
          isSupported = supported,
          parser = parseInterfaces
        }
{-# INLINEABLE netInterfaceShellApp #-}

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: (MonadPathReader m) => m Bool
supported = U.exeSupported [osp|ip|]
{-# INLINEABLE supported #-}

type MParser :: Type -> Type
type MParser = Parsec Void Text

-- | Attempts to parse the output of IP.
--
-- @since 0.1
parseInterfaces :: Text -> Either IpParseError NetInterfaces
parseInterfaces txt = case MP.parse mparseInterfaces "" txt of
  Left ex ->
    let prettyErr = MP.errorBundlePretty ex
     in Left $ MkIpParseError $ T.pack prettyErr
  Right ifs -> Right ifs
{-# INLINEABLE parseInterfaces #-}

mparseInterfaces :: MParser NetInterfaces
mparseInterfaces = MkNetInterfaces <$> MP.many parseInterface
{-# INLINEABLE mparseInterfaces #-}

parseInterface :: MParser NetInterface
parseInterface = do
  MP.takeWhile1P (Just "device num") Char.isDigit
  MPC.char ':'
  MPC.space
  device' <- MP.takeWhile1P (Just "device") (/= ':')
  MPC.char ':'
  MP.manyTill MP.anySingle (MPC.string "state ")
  state' <- parseNetInterfaceState
  U.takeLine
  MP.optional parseLink
  ipv4s' <- parseIpv4s
  ipv6s' <- parseIpv6s

  pure
    $ MkNetInterface
      { device = MkDevice device',
        ntype = Nothing,
        state = state',
        name = Nothing,
        ipv4s = MkIpAddresses ipv4s',
        ipv6s = MkIpAddresses ipv6s'
      }
{-# INLINEABLE parseInterface #-}

parseLink :: MParser ()
parseLink = MPC.space *> MPC.string "link" *> U.takeLine_
{-# INLINEABLE parseLink #-}

parseIpv4s :: MParser [IpAddress Ipv4]
parseIpv4s = parseIps "inet " MkIpAddress
{-# INLINEABLE parseIpv4s #-}

parseIpv6s :: MParser [IpAddress Ipv6]
parseIpv6s = parseIps "inet6 " MkIpAddress
{-# INLINEABLE parseIpv6s #-}

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
      -- 'many' will fail if we partially consume any input. We do not want
      -- a failure because we may be able to parse it with a later parser
      -- (i.e. ipv4 partially matches, fails, but ipv6 would succeed). Thus
      -- we include try to backtrack and give the ipv6 parser a chance.
      MP.try $ MPC.space *> MPC.string p
      MPC.space
      addr <- MP.takeWhile1P (Just "address") (/= '/')
      MPC.char '/'
      U.takeLine_
      lft
      pure addr
{-# INLINEABLE parseAddresses #-}

lft :: MParser ()
lft = do
  MPC.space
  MPC.string "valid_lft"
  U.takeLine_
{-# INLINEABLE lft #-}

parseNetInterfaceState :: MParser NetInterfaceState
parseNetInterfaceState = do
  MP.try up
    <|> down
    <|> unknown
    <?> "state"
  where
    up = MPC.string "UP" $> NetStateUp
    down = MPC.string "DOWN" $> NetStateDown
    unknown = NetStateUnknown <$> MP.takeWhile1P (Just "type") (not . Char.isSpace)
{-# INLINEABLE parseNetInterfaceState #-}
