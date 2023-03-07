module FindUsage where

import FindUsage.Definition (($%), Usage(Used), usageId, class UsageTC)
import FindUsage.Reexport2 (toBeReexported)
-- import Usage both as non-qualified and qualified
import FindUsage.Definition (Usage(..), usageId2) as D
import FindUsage.Definition as D -- test for duplicated imports


usagePatternMatch :: Usage -> D.Usage
usagePatternMatch x = case x of
  D.Used _ -> (x :: D.Usage) -- typed value
  _ $% _ -> x

-- without signature
usedVal = Used 0

usageFn :: ∀ a. a -> a
usageFn = usageId toBeReexported

-- assign to local
usageId2 :: forall a. a -> a
usageId2 = D.usageId2

-- use local
localUse :: Int
localUse = usageId2 10

newtype Find = Find Usage

instance UsageTC Find where
  use = Find