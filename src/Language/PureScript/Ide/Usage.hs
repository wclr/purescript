module Language.PureScript.Ide.Usage
  ( findUsages
  --, findReexportingModules
  --, directDependants
  --, eligibleModules
  --, applySearch
  ) where

import           Protolude hiding (moduleName)

import           Control.Lens (preview)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import           Language.PureScript.Ide.State (getAllModules, getFileState)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

import qualified Debug.Trace as Debug

-- |
-- How we find usages, given an IdeDeclaration and the module it was defined in:
--
-- 1. Find all modules that reexport the given declaration
-- 2. Find all modules that import from those modules, and while traversing the
-- imports build a specification for how the identifier can be found in the
-- module.
-- 3. Apply the collected search specifications and collect the results
findUsages
  :: Ide m
  => IdeDeclaration
  -> P.ModuleName
  -> m (ModuleMap (NonEmpty P.SourceSpan))
findUsages declaration moduleName = do
  ms <- getAllModules Nothing
  asts <- Map.map fst . fsModules <$> getFileState
  let elig = eligibleModules (moduleName, declaration) ms asts
  pure
    $ Map.mapMaybe nonEmpty
    $ Map.mapWithKey (\mn searches ->
        foldMap (applySearches searches) (Map.lookup mn asts)) elig

-- | A declaration can either be imported qualified, or unqualified. All the
-- information we need to find usages through a Traversal is thus captured in
-- the `Search` type.
type Search = P.Qualified IdeDeclaration

findReexportingModules
  :: (P.ModuleName, IdeDeclaration)
  -- ^ The declaration and the module it is defined in for which we are
  -- searching usages
  -> ModuleMap [IdeDeclarationAnn]
  -- ^ Our declaration cache. Needs to have reexports resolved
  -> [P.ModuleName]
  -- ^ All the modules that reexport the declaration. This does NOT include
  -- the defining module
findReexportingModules (moduleName, declaration) decls =
  Map.keys (Map.filter (any hasReexport) decls)
  where
    hasReexport d =
      (d & _idaDeclaration & identifierFromIdeDeclaration) == identifierFromIdeDeclaration declaration
      && (d & _idaAnnotation & _annExportedFrom) == Just moduleName
      && (d & _idaDeclaration & namespaceForDeclaration) == namespaceForDeclaration declaration

-- | Gets list of searches of the declaration in module.
searchesInModule :: IdeDeclaration -> P.Module -> P.ModuleName -> [Search]
searchesInModule declaration module' mn
  | P.getModuleName module' == mn = []
  | otherwise = foldMap isImporting (P.getModuleDeclarations module')
  where
    isImporting d = case d of
      P.ImportDeclaration _ mn' it qual | mn == mn' ->
        P.Qualified (P.byMaybeModuleName qual) <$> case it of
          P.Implicit -> pure declaration
          P.Explicit refs
            | any (declaration `matchesRef`) refs -> pure declaration
          P.Explicit _ -> []
          P.Hiding refs
            | not (any (declaration `matchesRef`) refs) -> pure declaration
          P.Hiding _ -> []
      _ -> []



matchesRef :: IdeDeclaration -> P.DeclarationRef -> Bool
matchesRef declaration ref =
  isJust $ matchedRefSpan declaration ref

-- | Determines whether an IdeDeclaration is referenced by a DeclarationRef
-- and returns a source span of the reference.
matchedRefSpan :: IdeDeclaration -> P.DeclarationRef -> Maybe P.SourceSpan
matchedRefSpan declaration ref = case declaration of
  IdeDeclValue valueDecl -> case ref of
    P.ValueRef sp i -> mbIf sp $ i == _ideValueIdent valueDecl
    _ -> Nothing
  IdeDeclType typeDecl -> case ref of
    P.TypeRef sp tn _ -> mbIf sp $ tn == _ideTypeName typeDecl
    _ -> Nothing
  IdeDeclTypeSynonym synonym -> case ref of
    P.TypeRef sp tn _ -> mbIf sp $ tn == _ideSynonymName synonym
    _ -> Nothing
  IdeDeclDataConstructor dtor -> case ref of
    P.TypeRef sp tn dtors
    -- We check if the given data constructor constructs the type imported
    -- here.
    -- This way we match `Just` with an import like `import Data.Maybe (Maybe(..))`
      | _ideDtorTypeName dtor == tn ->
          maybe (Just sp) (mbIf sp . elem (_ideDtorName dtor)) dtors
    _ -> Nothing
  IdeDeclTypeClass typeClass -> case ref of
    P.TypeClassRef sp name -> mbIf sp $ name == _ideTCName typeClass
    _ -> Nothing
  IdeDeclValueOperator valueOperator -> case ref of
    P.ValueOpRef sp opName -> mbIf sp $ opName == _ideValueOpName valueOperator
    _ -> Nothing
  IdeDeclTypeOperator typeOperator -> case ref of
    P.TypeOpRef sp opName -> mbIf sp $ opName == _ideTypeOpName typeOperator
    _ -> Nothing
  IdeDeclModule m -> case ref of
    P.ModuleRef sp mn -> mbIf sp $ m == mn
    _ -> Nothing
  where
    mbIf sp cond = if cond then Just sp else Nothing

-- | Filters modules that import the declaration (including reexports).
eligibleModules
  :: (P.ModuleName, IdeDeclaration)
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap P.Module
  -> ModuleMap (NonEmpty Search)
eligibleModules query@(moduleName, declaration) decls modules =
    Map.mapMaybe toSearches modules
      & Map.insert moduleName searchDefiningModule
  where
    searchDefiningModule = P.Qualified P.ByNullSourcePos declaration :| []
    importsToLookFor = moduleName :| findReexportingModules query decls

    toSearches m
      -- skip defining module
      | P.getModuleName m == moduleName  = Nothing
      -- nub searches because we may have duplicated imports
      | otherwise = nonEmpty $ ordNub $
        foldMap (searchesInModule declaration m)
        importsToLookFor

-- | Finds all usages for a given `Search`es throughout a module.
applySearches :: NonEmpty Search -> P.Module -> [P.SourceSpan]
applySearches searches module_ =
  foldMap findUsageInDeclaration decls
  where
    --decls = seq (Debug.traceShowId $ (P.getModuleName module_, length searches) ) P.getModuleDeclarations module_
    decls = P.getModuleDeclarations module_
    findUsageInDeclaration =
      let
        (extr, _, _, _, _) = P.everythingWithScope goDecl goExpr goBinder mempty mempty
      in
        extr mempty

    folds = flip foldMap searches

    goType ideType search t = case t of
      P.TypeConstructor (sp, _) tyName ->
          [sp | tyName == (search $> _ideTypeName ideType) ]
      _ -> []

    foldDataCtorTypes f (P.DataConstructorDeclaration _ _ vars) =
      foldMap (\(_, st) -> f st) vars

    sourceTypeSearch search =
      (\ideType -> P.everythingOnTypes (++) (goType ideType search))
        <$> preview _IdeDeclType (P.disqualify search)

    constraintTypes =
      foldMap (\c -> P.constraintArgs c <> P.constraintKindArgs c)

    qualBy (P.Qualified by _ ) = by

    goDecl _ decl = folds $ \search -> case decl of
      P.TypeDeclaration dt
        | Just ideType <- preview _IdeDeclType (P.disqualify search) ->
           P.everythingOnTypes (++) (goType ideType search) (P.tydeclType dt)
      P.DataDeclaration _ _ _ _ ctors
        | Just goSt <- sourceTypeSearch search ->
          foldMap (foldDataCtorTypes goSt) ctors
      P.TypeSynonymDeclaration _ _ _ st
        | Just goSt <- sourceTypeSearch search ->
          goSt st
      P.KindDeclaration _ _ _ st
        | Just goSt <- sourceTypeSearch search ->
          goSt st
      P.FixityDeclaration (sp, _) (Right (P.TypeFixity _ tn _))
        | Just ty <- preview _IdeDeclType (P.disqualify search) ->
          [sp  | (search $> _ideTypeName ty) == tn ]
      P.FixityDeclaration (sp, _) (Left (P.ValueFixity _ (P.Qualified qual val)  _))
        | qualBy search == qual ->
            case val of
              Right cn
                | Just ty <- preview _IdeDeclDataConstructor (P.disqualify search) ->
                [sp | _ideDtorName ty == cn ]
              Left i ->
                 [sp | P.runIdent i == identifierFromIdeDeclaration (P.disqualify search)]
              _ ->
                  []
        | otherwise ->
          []

      -- TODO: handle TC declarations, TC name
      P.TypeClassDeclaration _ _ _ cs _ _
        | Just goSt <- sourceTypeSearch search ->
          foldMap goSt (constraintTypes cs)
      -- TODO: handle instance body
      P.TypeInstanceDeclaration _ _ _ _ _ cs _ sts _
        | Just goSt <- sourceTypeSearch search ->
          foldMap goSt (constraintTypes cs ++ sts)
      -- TODO: constraintClass - no exact span too
      {- TypeInstanceDeclaration
        SourceAnn SourceAnn ChainId Integer (Either Text Ident)
        [SourceConstraint]
        (Qualified (ProperName 'ClassName))
        [SourceType]
        TypeInstanceBody
        -}
      P.TypeInstanceDeclaration (sp, _) _ _ _ _ _ tc _ _
        | Just ideTC <- preview _IdeDeclTypeClass (P.disqualify search) ->
          [sp | (search $> _ideTCName ideTC) == tc]

      -- TODO: search for type class methods?
      -- P.TypeInstanceDeclaration (sp, _) _ _ _ _ _ _ _ (P.ExplicitInstance decls) ->
      --   foldMap (goDecl scope) decls

      -- P.ValueDeclaration (P.ValueDeclarationData _ ident _ _ _) ->
      --   []

      P.ImportDeclaration _ _ it qual
        | P.getQual search == qual ->
          case it of
            P.Explicit refs ->
              foldRefs refs
            P.Hiding refs ->
              foldRefs refs
            _ ->
              []
          where
            foldRefs =
              foldMap (maybeToList . matchedRefSpan (P.disqualify search))
      _ -> []

    isLocal scope ident = P.LocalIdent ident `Set.member` scope

    goExpr scope expr = folds $ \search -> case expr of
      P.Var sp i
        | Just ideValue <- preview _IdeDeclValue (P.disqualify search)
        , P.isQualified search
          || not (isLocal scope (_ideValueIdent ideValue)) ->
          --[sp | Debug.traceShowId $ Debug.traceShowId (map P.runIdent i) == Debug.traceShowId (map identifierFromIdeDeclaration search)]
          [sp | map P.runIdent i == map identifierFromIdeDeclaration search]
      P.Constructor sp name
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search ->
          [sp | name == map _ideDtorName ideDtor]
      P.Op sp opName
        | Just ideOp <- traverse (preview _IdeDeclValueOperator) search ->
          [sp | opName == map _ideValueOpName ideOp]
      P.TypedValue _ _ st
        | Just goSt <- sourceTypeSearch search ->
          goSt st
      _ -> []

    goBinder _ binder = folds $ \search -> case binder of
      P.ConstructorBinder sp ctorName _
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search ->
          [sp | ctorName == map _ideDtorName ideDtor]
      P.OpBinder sp opName
        | Just op <- traverse (preview _IdeDeclValueOperator) search ->
          [sp | opName == map _ideValueOpName op]
      _ -> []
