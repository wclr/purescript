module Language.PureScript.Make.ExternsDiff
  ( ExternsDiff
  , diffsEffect
  , getExternsDiff
  ) where

import Protolude

import qualified Language.PureScript.AST as P
import qualified Language.PureScript.Externs as P
import qualified Data.List as L
import Debug.Trace (trace)

data ExternsDiff = ExternsDiff { unDiff :: Maybe [P.Declaration] }
  deriving (Show)

diffsEffect :: [ExternsDiff] -> P.Module -> Bool
diffsEffect diffs m =
  and (isNothing . unDiff <$> diffs)

getExternsDiff :: P.ExternsFile -> Maybe P.ExternsFile -> ExternsDiff
getExternsDiff ex1 ex2' = case ex2' of
  Just ex2 ->
    if eq P.efDeclarations then
      ExternsDiff Nothing
    else
      ExternsDiff (Just [])
    where
      eq fn = fn ex1 == fn ex2
      diff = foldl (flip L.delete) (P.efDeclarations ex1)  (P.efDeclarations ex2)
  Nothing ->
    ExternsDiff Nothing
