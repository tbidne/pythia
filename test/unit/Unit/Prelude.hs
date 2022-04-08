module Unit.Prelude
  ( unsafeIpAddress,
    module X,
  )
where

import Data.Either as X (isLeft)
import Pythia.Prelude as X
import Pythia.Services.Types.Network (IpAddress (..), IpRefinement)
import Refined (Predicate)
import Refined.Unsafe qualified as R
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

-- | Constructs an 'IpAddress', calls error if the refinement fails.
--
-- @since 0.1
unsafeIpAddress :: Predicate (IpRefinement a) Text => Text -> IpAddress a
unsafeIpAddress = MkIpAddress . R.unsafeRefine
