-- | Provides the 'BoundedNat' type for safe mathematical
-- operations.
module System.Info.Data.BoundedNat
  ( -- * Type
    BoundedNat (MkBoundedNat, unBoundedNat),

    -- * Creation
    mkBoundedNat,
    unsafeBoundedNat,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, Nat, natVal)

-- | Newtype wrapper over 'Natural'. The underlying 'Natural' is in \([l, u]\).
type BoundedNat :: Nat -> Nat -> Type
newtype BoundedNat l u = MkUnsafeBoundedNat
  { -- | Unwraps the 'BoundedNat'
    unBoundedNat :: Natural
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'BoundedNat'.
pattern MkBoundedNat :: Natural -> BoundedNat l u
pattern MkBoundedNat n <- MkUnsafeBoundedNat n

{-# COMPLETE MkBoundedNat #-}

instance (KnownNat l, KnownNat u) => Bounded (BoundedNat l u) where
  minBound = unsafeBoundedNat $ natVal $ Proxy @l
  maxBound = unsafeBoundedNat $ natVal $ Proxy @u

-- | Contructs a 'BoundedNat'.
--
-- >>> mkBoundedNat @0 @100 50
-- Just (MkUnsafeBoundedNat {unBoundedNat = 50})
--
-- >>> mkBoundedNat @10 @20 25
-- Nothing
mkBoundedNat :: forall l u. (KnownNat l, KnownNat u) => Natural -> Maybe (BoundedNat l u)
mkBoundedNat n
  | n >= lower && n <= upper = Just $ MkUnsafeBoundedNat n
  | otherwise = Nothing
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u

-- | Unsafe constructor for 'BoundedNat', intended to be used with
-- known constants, e.g. @unsafeBoundedNat \@0 \@100 50@. Exercise restraint!
unsafeBoundedNat :: forall l u. (KnownNat l, KnownNat u) => Natural -> BoundedNat l u
unsafeBoundedNat n
  | n >= lower && n <= upper = MkUnsafeBoundedNat n
  | otherwise =
    error $
      "Passed invalid "
        <> show n
        <> " bounded by ["
        <> show lower
        <> ","
        <> show upper
        <> "]"
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u
