module FindUsage.Reexport (module X) where

import FindUsage.Definition (toBeReexported) as X

re :: Int
re = X.toBeReexported 10
