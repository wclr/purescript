module FindUsage.Reexport2 (module X) where

import FindUsage.Reexport as X

re :: Int
re = X.toBeReexported 10
