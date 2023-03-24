module Language.PureScript.Make.ExternsDiff
  ( ExternsDiff
  , emptyDiff
  , checkDiffs
  , diffExterns
  ) where

import Protolude

import Data.List qualified as L
import Language.PureScript.AST qualified as P
import Language.PureScript.Externs qualified as P

import Debug.Trace (trace)

data ExternsDiff = ExternsDiff {unDiff :: Maybe [P.Declaration]}
  deriving (Show)

-- | Empty diff means no effective difference between externs exist
emptyDiff :: ExternsDiff
emptyDiff = ExternsDiff Nothing

isEmpty :: ExternsDiff -> Bool
isEmpty =
  isNothing . unDiff

checkDiffs :: P.Module -> [ExternsDiff] -> Bool
checkDiffs _ diffs =
  and (isEmpty <$> diffs)

diffExterns :: P.ExternsFile -> P.ExternsFile -> ExternsDiff
diffExterns ex1 ex2 =
  if eq P.efDeclarations
    then emptyDiff
    else ExternsDiff (Just [])
  where
    eq fn = fn ex1 == fn ex2
    diff = foldl (flip L.delete) (P.efDeclarations ex1) (P.efDeclarations ex2)
