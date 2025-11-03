-- | Provides the 'Percentage' type.
--
-- @since 0.1
module Pythia.Data.Percentage
  ( -- * Type
    Percentage (MkPercentage),

    -- * Creation
    mkPercentage,
    mkPercentageTH,
    unsafePercentage,

    -- * Elimination
    unPercentage,

    -- * Optics
    _MkPercentage,
  )
where

import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Numeric.Data.Interval qualified as Interval
import Optics.Core (ReversedPrism')
import Pythia.Data.Percentage.Internal
  ( Percentage (InternalPercentage, MkPercentage),
    unPercentage,
  )
import Pythia.Prelude

-- | Creates a percentage for x in [0, 100].
--
-- @since 0.1
mkPercentage :: Word8 -> Either String Percentage
mkPercentage = fmap InternalPercentage . Interval.mkInterval

-- | Safely creates a percentage at compile-time.
--
-- @since 0.1
mkPercentageTH :: Word8 -> Code Q Percentage
mkPercentageTH x = either (error . errMsg x) liftTyped $ mkPercentage x

-- | Unsafely creates a percentage for x in [0, 100]. Calls error otherwise.
--
-- @since 0.1
unsafePercentage :: (HasCallStack) => Word8 -> Percentage
unsafePercentage x = either (error . errMsg x) id $ mkPercentage x

-- | 'ReversedPrism'' that enables total elimination and partial construction.
--
-- @since 0.1
_MkPercentage :: ReversedPrism' Percentage Word8
_MkPercentage = re (prism unPercentage g)
  where
    g x = case mkPercentage x of
      Left _ -> Left x
      Right x' -> Right x'

-- Ignore smart-math error in favor of slightly clearer one.
errMsg :: Word8 -> String -> String
errMsg x _ =
  "Pythia.Data.Percentage: Wanted percentage in [0, 100], received: " <> show x
