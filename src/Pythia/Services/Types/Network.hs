{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides common network types.
--
-- @since 0.1
module Pythia.Services.Types.Network
  ( -- * IP Types
    IpType (..),
    IpAddress (..),

    -- ** Refinements
    IpRefinement,
    Ipv4Refinement,
    Ipv6Refinement,

    -- * Network Device
    Device (..),
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Typeable (typeRep)
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Prelude
import Refined (Predicate, Refined)
import Refined qualified as R

-- $setup
-- >>> :{
--   let trim :: Show a => a -> String
--       trim = T.unpack . T.strip . T.pack . show
-- :}

-- | Newtype wrapper over a network device name.
--
-- @since 0.1
newtype Device = MkDevice
  { -- | @since 0.1
    unDevice :: Text
  }
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Read,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      IsString,
      -- | @since 0.1
      PrettyPrinter
    )
    via Text

-- | @since 0.1
makeFieldLabelsNoPrefix ''Device

-- | IP types.
--
-- @since 0.1
data IpType
  = -- | @since 0.1
    Ipv4
  | -- | @since 0.1
    Ipv6
  deriving (Eq, Show)

-- | Maps 'IpType' to its 'Text' refinement.
--
-- @since 0.1
type IpRefinement :: IpType -> Type
type family IpRefinement a where
  IpRefinement 'Ipv4 = Ipv4Refinement
  IpRefinement 'Ipv6 = Ipv6Refinement

-- | IPv4 Refinement. We implement a custom type here so we get better error
-- messages. 'Text' must satisfy:
--
-- * Length in @(0, 16)@.
-- * Chars are all decimal digits or dots.
--
-- ==== __Examples__
-- >>> R.refine @Ipv4Refinement @Text "192.168.1.2"
-- Right (Refined "192.168.1.2")
--
-- >>> trim $ R.refine @Ipv4Refinement @Text ""
-- "Left   The predicate (Ipv4Refinement) failed with the message: Received empty IPv4 address. Should have length (0, 16)."
--
-- >>> trim $ R.refine @Ipv4Refinement @Text "192.168.111.222.7"
-- "Left   The predicate (Ipv4Refinement) failed with the message: Invalid IPv4 length: <17> for ip text: <192.168.111.222.7>. Should be in (0, 16)."
--
-- >>> trim $ R.refine @Ipv4Refinement @Text "192.168x1.2"
-- "Left   The predicate (Ipv4Refinement) failed with the message: Invalid IPv4 content: <192.168x1.2>. Should only contain decimal digits or dots."
--
-- @since 0.1
data Ipv4Refinement

-- | @since 0.1
instance Predicate Ipv4Refinement Text where
  validate p txt
    | not validLen = R.throwRefineOtherException (typeRep p) errLen
    | not validChars = R.throwRefineOtherException (typeRep p) errChars
    | otherwise = Nothing
    where
      len = T.length txt
      validLen = len > 0 && len < 16
      validChars = T.all (\c -> Char.isDigit c || c == '.') txt
      errLen =
        if len == 0
          then "Received empty IPv4 address. Should have length (0, 16)."
          else
            "Invalid IPv4 length: <" <> showt len
              <> "> for ip text: <"
              <> txt
              <> ">. Should be in (0, 16)."
      errChars =
        "Invalid IPv4 content: <" <> txt
          <> ">. Should only contain decimal digits or dots."

-- | IPv6 Refinement. We implement a custom type here so we get better error
-- messages. 'Text' must satisfy:
--
-- * Length in @(0, 40)@.
-- * Chars are all hex digits or colons.
--
-- ==== __Examples__
-- >>> R.refine @Ipv6Refinement @Text "fe80::a328:482:5263:10b8"
-- Right (Refined "fe80::a328:482:5263:10b8")
--
-- >>> trim $ R.refine @Ipv6Refinement @Text ""
-- "Left   The predicate (Ipv6Refinement) failed with the message: Received empty IPv6 address. Should have length (0, 40)."
--
-- >>> trim $ R.refine @Ipv6Refinement @Text "fe80:a328:4822:5263:10b8:4062:10d3:16ac:"
-- "Left   The predicate (Ipv6Refinement) failed with the message: Invalid IPv6 length: <40> for ip text: <fe80:a328:4822:5263:10b8:4062:10d3:16ac:>. Should be in (0, 40)."
--
-- >>> trim $ R.refine @Ipv6Refinement @Text "fe80::a328:482:5263:10b8x"
-- "Left   The predicate (Ipv6Refinement) failed with the message: Invalid IPv6 content: <fe80::a328:482:5263:10b8x>. Should only contain hex digits or colons."
--
-- @since 0.1
data Ipv6Refinement

-- | @since 0.1
instance Predicate Ipv6Refinement Text where
  validate p txt
    | not validLen = R.throwRefineOtherException (typeRep p) errLen
    | not validChars = R.throwRefineOtherException (typeRep p) errChars
    | otherwise = Nothing
    where
      len = T.length txt
      validLen = len > 0 && len < 40
      validChars = T.all (\c -> Char.isHexDigit c || c == ':') txt
      errLen =
        if len == 0
          then "Received empty IPv6 address. Should have length (0, 40)."
          else
            "Invalid IPv6 length: <" <> showt len
              <> "> for ip text: <"
              <> txt
              <> ">. Should be in (0, 40)."
      errChars =
        "Invalid IPv6 content: <" <> txt
          <> ">. Should only contain hex digits or colons."

-- | Type for an IP address. The type family 'IpRefinement' refines the
-- underlying 'Text' according to the spec.
--
-- * 'Ipv4': All characters are digits or dots, and the length is
--           @0 < l < 16@.
-- * 'Ipv6': All characters are hex digits or colons, and the length is
--           @0 < l < 40@.
--
-- @since 0.1
newtype IpAddress a = MkIpAddress
  { -- | @since 0.1
    unIpAddress :: Refined (IpRefinement a) Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance PrettyPrinter (IpAddress a) where
  pretty = R.unrefine . unIpAddress

-- | @since 0.1
makeFieldLabelsNoPrefix ''IpAddress
