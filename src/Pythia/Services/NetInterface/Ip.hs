{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving network connection
-- information using ip utility.
--
-- @since 0.1
module Pythia.Services.NetInterface.Ip
  ( -- * Query
    netInterfaceShellApp,
    supported,

    -- * Misc
    IpException (..),
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
    NetInterfaces (..),
  )
import Pythia.Services.Types.Network (Device (..), IpAddress (..), IpType (..))
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R
import Text.Megaparsec (ErrorFancy (..), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> import GHC.Exception (errorCallException)

-- | Errors that can occur with ip.
--
-- ==== __Examples__
--
-- >>> putStrLn $ displayException $ IpGeneralException $ errorCallException "oh no"
-- Ip exception: <oh no>
--
-- >>> putStrLn $ displayException $ IpParseException "parse error"
-- Ip parse exception: <parse error>
--
-- @since 0.1
data IpException
  = -- | General exceptions.
    --
    -- @since 0.1
    forall e. Exception e => IpGeneralException e
  | -- | Parse exception.
    --
    -- @since 0.1
    IpParseException Text

-- | @since 0.1
makePrismLabels ''IpException

-- | @since 0.1
deriving stock instance Show IpException

-- | @since 0.1
instance PrettyPrinter IpException where
  pretty (IpGeneralException e) = "Ip exception: <" <> T.pack (displayException e) <> ">"
  pretty (IpParseException s) = "Ip parse exception: <" <> s <> ">"

-- | @since 0.1
instance Exception IpException where
  displayException = T.unpack . pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | Ip query for 'NetInterface'.
--
-- __Throws:__
--
-- * 'IpException': if something goes wrong (i.e. exception while running
--       the command, or we have a parse error).
--
-- @since 0.1
netInterfaceShellApp :: (MonadCatch m, MonadIO m) => m NetInterfaces
netInterfaceShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "ip address",
          parser = parseInterfaces,
          liftShellEx = IpGeneralException
        }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: MonadIO m => m Bool
supported = U.exeSupported "ip"

type MParser = Parsec Void Text

-- | Attempts to parse the output of IP.
--
-- @since 0.1
parseInterfaces :: Text -> Either IpException NetInterfaces
parseInterfaces txt = case MP.parse mparseInterfaces "" txt of
  Left ex ->
    let prettyErr = MP.errorBundlePretty ex
     in Left $ IpParseException $ T.pack prettyErr
  Right ifs -> Right ifs

mparseInterfaces :: MParser NetInterfaces
mparseInterfaces = MkNetInterfaces <$> MP.many parseInterface

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

  pure $
    MkNetInterface
      { idevice = MkDevice device',
        itype = Nothing,
        istate = state',
        iname = Nothing,
        ipv4s = ipv4s',
        ipv6s = ipv6s'
      }

parseLink :: MParser ()
parseLink = MPC.space *> MPC.string "link" *> U.takeLine_

parseIpv4s :: MParser [IpAddress 'Ipv4]
parseIpv4s = parseIps "inet " MkIpAddress

parseIpv6s :: MParser [IpAddress 'Ipv6]
parseIpv6s = parseIps "inet6 " MkIpAddress

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

lft :: MParser ()
lft = do
  MPC.space
  MPC.string "valid_lft"
  U.takeLine_

parseNetInterfaceState :: MParser NetInterfaceState
parseNetInterfaceState = do
  MP.try up
    <|> down
    <|> unknown
    <?> "state"
  where
    up = MPC.string "UP" $> Up
    down = MPC.string "DOWN" $> Down
    unknown = UnknownState <$> MP.takeWhile1P (Just "type") (not . Char.isSpace)
