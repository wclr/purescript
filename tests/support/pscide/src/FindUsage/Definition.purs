module FindUsage.Definition
  ( Usage(..)
  , usageId
  , usageId2
  , class UsageTC
  , use
  , ($%)
  , toBeReexported
  ) where

data Usage
  = Used Int
  | Usage Int Int

infixl 2 Usage as $%

usageId :: ∀ a. a -> a
usageId x = x

usageId2 :: ∀ a. a -> a
usageId2 x = x

toBeReexported :: ∀ a. a -> a
toBeReexported = usageId

localId :: ∀ a. a -> a
localId x = x

loc :: Int
loc = localId 10

type Usages = Array Usage

infixl 2 type Usage as <$%>

class UsageTC a where
  use :: Usage -> a