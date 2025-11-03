{-# LANGUAGE CPP #-}
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

    -- * Optics
    _Ipv4,
    _Ipv6,
  )
where

import Data.Aeson (FromJSON)
import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Typeable (typeRep)
#if MIN_VERSION_base(4,17,0)
import GHC.IsList (IsList (Item, fromList, toList))
#else
import GHC.Exts (IsList (Item, fromList, toList))
#endif
import Pythia.Prelude
import Refined (Predicate, Refined)
import Refined qualified as R

-- $setup
-- >>> import Data.Text qualified as T
-- >>> import Pythia.Prelude
-- >>> import Refined qualified as R
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
      Display,
      -- | @since 0.1
      FromJSON,
      -- | @since 0.1
      IsString
    )
    via Text
  deriving anyclass
    ( -- | @since 0.1.0.0
      Hashable,
      -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unDevice" k Device Device a b
  where
  labelOptic = iso (\(MkDevice d) -> d) MkDevice
  {-# INLINE labelOptic #-}

-- | IP types.
--
-- @since 0.1
type IpType :: Type
data IpType
  = -- | @since 0.1
    Ipv4
  | -- | @since 0.1
    Ipv6
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
    ( -- | @since 0.1.0.0
      Hashable,
      -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
_Ipv4 :: Prism' IpType ()
_Ipv4 =
  prism
    (const Ipv4)
    ( \x -> case x of
        Ipv4 -> Right ()
        _ -> Left x
    )
{-# INLINE _Ipv4 #-}

-- | @since 0.1
_Ipv6 :: Prism' IpType ()
_Ipv6 =
  prism
    (const Ipv6)
    ( \x -> case x of
        Ipv6 -> Right ()
        _ -> Left x
    )
{-# INLINE _Ipv6 #-}

-- | Maps 'IpType' to its 'Text' refinement.
--
-- @since 0.1
type IpRefinement :: IpType -> Type
type family IpRefinement a where
  IpRefinement Ipv4 = Ipv4Refinement
  IpRefinement Ipv6 = Ipv6Refinement

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
-- * 'Ipv4': All characters are digits or dots, and the length is
--           @0 < l < 16@.
-- * 'Ipv6': All characters are hex digits or colons, and the length is
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
    ( -- | @since 0.1.0.0
      Hashable,
      -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Refined (IpRefinement s) Text, b ~ Refined (IpRefinement s) Text) =>
  LabelOptic "unIpAddress" k (IpAddress s) (IpAddress s) a b
  where
  labelOptic = iso (\(MkIpAddress a) -> a) MkIpAddress
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Display (IpAddress a) where
  displayBuilder = displayBuilder . R.unrefine . unIpAddress

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
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via [IpAddress a]
  deriving anyclass
    ( -- | @since 0.1.0.0
      Hashable,
      -- | @since 0.1.0.0
      NFData
    )

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ [IpAddress s], b ~ [IpAddress s]) =>
  LabelOptic "unIpAddresses" k (IpAddresses s) (IpAddresses s) a b
  where
  labelOptic = iso (\(MkIpAddresses a) -> a) MkIpAddresses
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Display (IpAddresses a) where
  displayBuilder = displayList . view #unIpAddresses

-- | @since 0.1
instance IsList (IpAddresses a) where
  type Item (IpAddresses a) = IpAddress a

  fromList = MkIpAddresses
  toList = unIpAddresses
