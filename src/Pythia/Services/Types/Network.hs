{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides common network types.
--
-- @since 0.1
module Pythia.Services.Types.Network
  ( -- * IP Types
    IpType (..),
    IpAddress (..),
    IpAddresses (..),

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
import Pythia.Prelude
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U
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
type Device :: Type
newtype Device = MkDevice
  { -- | @since 0.1
    unDevice :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
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
      Pretty
    )
    via Text
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

makePrismLabels ''Device

-- | IP types.
--
-- @since 0.1
type IpType :: Type
data IpType
  = -- | @since 0.1
    IpTypeIpv4
  | -- | @since 0.1
    IpTypeIpv6
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrismLabels ''IpType

-- | Maps 'IpType' to its 'Text' refinement.
--
-- @since 0.1
type IpRefinement :: IpType -> Type
type family IpRefinement a where
  IpRefinement 'IpTypeIpv4 = Ipv4Refinement
  IpRefinement 'IpTypeIpv6 = Ipv6Refinement

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
-- "Left   The predicate (Ipv4Refinement) failed with the message: Expected IPv4 with length (0, 16). Received empty."
--
-- >>> trim $ R.refine @Ipv4Refinement @Text "192.168.111.222.7"
-- "Left   The predicate (Ipv4Refinement) failed with the message: Expected IPv4 address with length (0, 16). Received '192.168.111.222.7' of length 17"
--
-- >>> trim $ R.refine @Ipv4Refinement @Text "192.168x1.2"
-- "Left   The predicate (Ipv4Refinement) failed with the message: IPv4 address should only contain decimal digits or dots. Received invalid: 192.168x1.2"
--
-- @since 0.1
type Ipv4Refinement :: Type
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
          then "Expected IPv4 with length (0, 16). Received empty."
          else
            "Expected IPv4 address with length (0, 16). Received '"
              <> txt
              <> "' of length "
              <> showt len
      errChars =
        "IPv4 address should only contain decimal digits or dots. Received invalid: "
          <> txt
  {-# INLINEABLE validate #-}

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
-- "Left   The predicate (Ipv6Refinement) failed with the message: Expected IPv6 of length (0, 40). Received empty."
--
-- >>> trim $ R.refine @Ipv6Refinement @Text "fe80:a328:4822:5263:10b8:4062:10d3:16ac:"
-- "Left   The predicate (Ipv6Refinement) failed with the message: Expected IPv6 with length (0, 40). Received 'fe80:a328:4822:5263:10b8:4062:10d3:16ac:' of length 40"
--
-- >>> trim $ R.refine @Ipv6Refinement @Text "fe80::a328:482:5263:10b8x"
-- "Left   The predicate (Ipv6Refinement) failed with the message: IPv6 address should only contain hex digits or colons. Received invalid: fe80::a328:482:5263:10b8x"
--
-- @since 0.1
type Ipv6Refinement :: Type
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
          then "Expected IPv6 of length (0, 40). Received empty."
          else
            "Expected IPv6 with length (0, 40). Received '"
              <> txt
              <> "' of length "
              <> showt len
      errChars =
        "IPv6 address should only contain hex digits or colons. Received invalid: "
          <> txt
  {-# INLINEABLE validate #-}

-- | Type for an IP address. The type family 'IpRefinement' refines the
-- underlying 'Text' according to the spec.
--
-- * 'IpTypeIpv4': All characters are digits or dots, and the length is
--           @0 < l < 16@.
-- * 'IpTypeIpv6': All characters are hex digits or colons, and the length is
--           @0 < l < 40@.
--
-- @since 0.1
type IpAddress :: IpType -> Type
newtype IpAddress a = MkIpAddress
  { -- | @since 0.1
    unIpAddress :: Refined (IpRefinement a) Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- Generating with makePrismLabels ''IpAddress fails to typecheck.
-- In particular, it appears like it's trying to generate an instance for
-- different types (IpAddress i) (IpAddress j), which causes an error
-- "liberal coverage condition fails". Thus manually writing it instead.

-- | @since 0.1
instance
  ( k ~ An_Iso,
    a ~ Refined (IpRefinement i) Text,
    b ~ Refined (IpRefinement i) Text
  ) =>
  LabelOptic "_MkIpAddress" k (IpAddress i) (IpAddress i) a b
  where
  labelOptic = iso unIpAddress MkIpAddress
  {-# INLINEABLE labelOptic #-}

-- | @since 0.1
instance Pretty (IpAddress a) where
  pretty = pretty . R.unrefine . unIpAddress
  {-# INLINEABLE pretty #-}

-- | @since 0.1
type IpAddresses :: IpType -> Type
newtype IpAddresses a = MkIpAddresses
  { -- | @since 0.1
    unIpAddresses :: [IpAddress a]
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrismLabels ''IpAddresses

-- | @since 0.1
instance Semigroup (IpAddresses a) where
  MkIpAddresses xs <> MkIpAddresses ys = MkIpAddresses (xs <> ys)
  {-# INLINEABLE (<>) #-}

-- | @since 0.1
instance Monoid (IpAddresses a) where
  mempty = MkIpAddresses []
  {-# INLINEABLE mempty #-}

-- | @since 0.1
instance Pretty (IpAddresses a) where
  pretty = U.hsep . U.punctuate U.comma . fmap pretty . view #unIpAddresses
  {-# INLINEABLE pretty #-}
