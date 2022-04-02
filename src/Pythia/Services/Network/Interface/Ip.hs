{-# LANGUAGE DeriveAnyClass #-}

-- | This module provides functionality for retrieving network connection
-- information using ip utility.
--
-- @since 0.1.0.0
module Pythia.Services.Network.Interface.Ip
  ( -- * Query
    netInterfaceShellApp,
    supported,

    -- * Misc
    IpError (..),
    parseInterfaces,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as T
import Pythia.Prelude
import Pythia.Services.Network.Interface.Types
  ( Interface (..),
    InterfaceState (..),
    Interfaces (..),
  )
import Pythia.Services.Network.Types (Device (..), Ipv4Address (..), Ipv6Address (..))
import Pythia.ShellApp (CmdError (..), SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Refined (Predicate, Refined)
import Refined qualified as R
import Text.Megaparsec (ErrorFancy (..), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Ip query for 'Interface'.
--
-- @since 0.1.0.0
netInterfaceShellApp :: (Throws CmdError, Throws IpError) => IO Interfaces
netInterfaceShellApp =
  ShellApp.runSimple $
    MkSimpleShell
      { command = "ip address",
        parser = parseInterfaces
      }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1.0.0
supported :: IO Bool
supported = U.exeSupported "ip"

type MParser = Parsec Void Text

-- | Attempts to parse the output of IP.
--
-- @since 0.1.0.0
parseInterfaces :: Text -> Either IpError Interfaces
parseInterfaces txt = case MP.parse mparseInterfaces "" txt of
  Left ex ->
    let prettyErr = MP.errorBundlePretty ex
     in Left $ IpParseErr prettyErr
  Right ifs -> Right ifs

mparseInterfaces :: MParser Interfaces
mparseInterfaces = MkInterfaces <$> MP.many parseInterface

parseInterface :: MParser Interface
parseInterface = do
  MP.takeWhile1P (Just "device num") Char.isDigit
  MPC.char ':'
  MPC.space
  device' <- MP.takeWhile1P (Just "device") (/= ':')
  MPC.char ':'
  MP.manyTill MP.anySingle (MPC.string "state ")
  state' <- parseInterfaceState
  U.takeLine
  MP.optional parseLink
  ipv4s' <- parseIpv4s
  ipv6s' <- parseIpv6s

  pure $
    MkInterface
      { idevice = MkDevice device',
        itype = Nothing,
        istate = state',
        iname = Nothing,
        ipv4s = ipv4s',
        ipv6s = ipv6s'
      }

parseLink :: MParser ()
parseLink = MPC.space *> MPC.string "link" *> U.takeLine_

parseIpv4s :: MParser [Ipv4Address]
parseIpv4s = parseIps "inet " MkIpv4Address

parseIpv6s :: MParser [Ipv6Address]
parseIpv6s = parseIps "inet6 " MkIpv6Address

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

parseInterfaceState :: MParser InterfaceState
parseInterfaceState = do
  MP.try up
    <|> down
    <|> unknown
    <?> "state"
  where
    up = MPC.string "UP" $> Up
    down = MPC.string "DOWN" $> Down
    unknown = UnknownState <$> MP.takeWhile1P (Just "type") (not . Char.isSpace)

-- | Errors that can occur when running the \'ip\' command.
--
-- @since 0.1.0.0
newtype IpError
  = -- | Parse error.
    --
    -- @since 0.1.0.0
    IpParseErr String
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass (Exception)
