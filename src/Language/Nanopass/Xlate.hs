{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nanopass.Xlate
  ( mkXlate
  , declareXlate
  , XlateDef(..)
  , XlateProd
  , XlateAuto(..)
  , XlateHoleDef(..)
  , XlateSyncatDef(..)
  ) where

import Language.Nanopass.LangDef

import Control.Monad (forM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Either (lefts)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Language.Haskell.TH (Exp(AppE,VarE))
import Language.Haskell.TH (Q,Dec)
import Language.Haskell.TH (Type(AppT))

import qualified Control.Monad.Trans as M
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH


mkXlate :: DefdLang -> DefdLang -> Q [Dec]
mkXlate l1 l2 = xlateDef l1 l2 >>= declareXlate l1 l2

declareXlate :: DefdLang -> DefdLang -> XlateDef -> Q [Dec]
declareXlate l1 l2 xlate = do
  xlateType <- declareType xlate
  descends <- defineDescend l1 l2 xlate
  pure $ xlateType : descends

---------------------------------------------
------ Gather Translation Requirements ------
---------------------------------------------

data XlateDef = XlateDef
  { xlateParams :: [TH.Name] -- ^ the type parameters of both languages, merged
  , xlateFParam :: TH.Name -- ^ a type for an Applicative parameter
  , xlateSyncats :: [XlateSyncatDef]
    -- ^ information about the syntactic cateories shared by both source and target
    -- this is used to allow users to override the bahavior of automatic translation
  , xlateProds :: [XlateProd] -- FIXME these should go under xlateSyncats, probly
    -- ^ information about the productions in the source that are missing in the target
    -- this is so that we require the user to supply these in an Xlate type
  }
type XlateProd = Either XlateHoleDef XlateAuto
data XlateAuto = XlateAuto
  { syncatName :: String
  , prodName :: String
  , autoArgs :: [TH.Name -> TH.Name -> Exp] -- functions from xlate and subterm variables to auto-translator
  }
data XlateHoleDef = XlateHoleDef
  { syncatName :: String -- the name of the syntactic category shared by source and target
  , prodName :: String -- the name of the source production
  , holeArgs :: [TH.Type] -- the types of the subterms of the source production
  , holeResult :: TH.Type -- the type of the target syntactic category that must be supplied
  }
data XlateSyncatDef = XlateSyncatDef
  { syncatName :: String -- the name of the syntactic category shared by source and target
  , fromType :: TH.Type -- parameterized type of the source language at this syntactic category
  , toType :: TH.Type -- parameterized type of the target language at this syntactic category
  }

xlateDef :: DefdLang -> DefdLang -> Q XlateDef
xlateDef l1 l2 = do
  let xlateParams = nub (l1.defdLangParams ++ l2.defdLangParams)
  xlateFParam <- if TH.mkName "f" `elem` xlateParams
    then TH.newName "f"
    else pure $ TH.mkName "f"
  xlateProds <- fmap concat $ forM (Map.toAscList $ l1.defdSyncats) $ detectHoles xlateFParam l1 l2
  let xlateSyncats = concatMap (detectOverrides xlateFParam l1 l2) $ Map.toAscList l1.defdSyncats
  pure $ XlateDef
    { xlateParams
    , xlateFParam
    , xlateSyncats
    , xlateProds
    }

detectHoles :: TH.Name -> DefdLang -> DefdLang -> (String, DefdSyncatType) -> Q [Either XlateHoleDef XlateAuto]
detectHoles fParam l1 l2 (sName, s1) = case Map.lookup sName l2.defdSyncats of
  Nothing -> pure [] -- no translation required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just s2 -> fmap concat $ forM (Map.toAscList s1.defdProds) $ detectHoleCtors s2
  where
  detectHoleCtors :: DefdSyncatType -> (String, DefdProd) -> Q [Either XlateHoleDef XlateAuto]
  detectHoleCtors s2 (pName, prod1) = case Map.lookup pName s2.defdProds of
    -- a required hole, because there is no constructor to target
    Nothing -> pure [Left $ createHole pName prod1]
    Just prod2
      -- no custom translation required: the arguments of one constructor match up with the arguments of the other
      | tys1 <- (defdSubtermType <$> prod1.defdSubterms)
      , tys2 <- (defdSubtermType <$> prod2.defdSubterms)
      , tys1 == tys2 -> runMaybeT (createAuto `mapM` tys1) >>= \case
          Nothing -> pure [Left $ createHole pName prod1] -- a required hole because no auto-translation possible
          Just autoArgs -> do
            pure [Right XlateAuto{syncatName=sName,prodName=pName,autoArgs}]
      -- a required hole, because the arguments of the constructors do not have the same structure
      | otherwise  -> pure [Left $ createHole pName prod1]
  createHole pName prod1 =
    let holeArgs = flip map (defdSubterms prod1) $ \subterm ->
          interpretTypeDesc l1 subterm.defdSubtermType
        holeCtor = TH.ConT (TH.mkName $ l2.langQualPrefix ++ sName)
        holeCore = foldl AppT holeCtor (TH.VarT <$> l2.defdLangParams)
        holeResult = AppT (TH.VarT fParam) holeCore
     in XlateHoleDef{syncatName=sName,prodName=pName,holeArgs,holeResult}

detectOverrides :: TH.Name -> DefdLang -> DefdLang -> (String, DefdSyncatType) -> [XlateSyncatDef]
detectOverrides fParam l1 l2 (sName, _) = case Map.lookup sName l2.defdSyncats of
  Nothing -> [] -- no translation required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just _ ->
    let fromTypeCtor = TH.ConT (TH.mkName $ l1.langQualPrefix ++ sName)
        fromType = foldl AppT fromTypeCtor (TH.VarT <$> l1.defdLangParams)
        toTypeCtor = TH.ConT (TH.mkName $ l2.langQualPrefix ++ sName)
        toTypeCore = foldl AppT toTypeCtor (TH.VarT <$> l2.defdLangParams)
        toType = AppT (TH.ConT ''Maybe) $ AppT (TH.VarT fParam) toTypeCore
     in [XlateSyncatDef{syncatName = sName,fromType,toType}]

createAuto :: TypeDesc -> MaybeT Q (TH.Name -> TH.Name -> Exp)
createAuto (RecursiveType sName) = do
  let repName = TH.mkName $ "descend" ++ sName
      auto xlateVar argVar = VarE repName `AppE` VarE xlateVar `AppE` VarE argVar
  pure auto
createAuto (VarType _) = do
  let auto _ argVar = VarE 'pure `AppE` VarE argVar
  pure auto
createAuto (CtorType tyName ts)
  | all (not . containsGrammar) ts = do
    let auto _ argVar = VarE 'pure `AppE` VarE argVar
    pure auto
  | t:ts' <- reverse ts
  , all (not . containsGrammar) ts' = do
      let travCandidate = foldl AppT (TH.ConT tyName) (interpretTypeDesc undefined <$> ts')
      isTraversable <- M.lift $ TH.isInstance ''Traversable [travCandidate]
      if isTraversable then traversableAuto t else hoistNothing
  -- TODO maybe try Bitraversable
  | otherwise = hoistNothing
createAuto (ListType t) = traversableAuto t
createAuto (MaybeType t) = traversableAuto t
createAuto (NonEmptyType t) = traversableAuto t
createAuto (TupleType t1 t2 ts) = do
  tupleMaker <- do
    tVars <- forM [1..length (t1:t2:ts)] $ \i -> M.lift $ TH.newName ("t" ++ show i)
    pure $ TH.LamE (TH.VarP <$> tVars) $ TH.TupE (Just . VarE <$> tVars)
  (args', autos') <- fmap unzip $ forM (zip [(1::Int)..] (t1:t2:ts)) $ \(i, t) -> do
    auto' <- createAuto t
    arg' <- M.lift $ TH.newName ("a" ++ show i)
    pure (arg', auto')
  let auto xlateVar argVar =
        let elemAuto auto' arg' = auto' xlateVar arg'
            lam = TH.LamE [TH.TupP $ TH.VarP <$> args'] $
              foldl idiomAppE (AppE (VarE 'pure) tupleMaker) (zipWith elemAuto autos' args')
         in lam `AppE` VarE argVar
  pure auto
createAuto (MapType k v)
  | not (containsGrammar k) = traversableAuto v
  | otherwise = hoistNothing

traversableAuto :: TypeDesc -> MaybeT Q (TH.Name -> TH.Name -> Exp)
traversableAuto t = do
  var <- M.lift $ TH.newName "x"
  auto' <- createAuto t
  let auto xlateVar argVar =
        let lam = TH.LamE [TH.VarP var] (auto' xlateVar var)
         in VarE 'traverse `AppE` lam `AppE` VarE argVar
  pure auto


---------------------------------
------ Declare XLate Types ------
---------------------------------

declareType :: XlateDef -> Q Dec
declareType x = do
  TH.addModFinalizer $ TH.putDoc (TH.DeclDoc xlateName) $ unlines
    [ "This type is used to parameterize the nanopass-generated translation functions @descend*@."
    , "It has members for:"
    , ""
    , "  * each constructor that could not be translated"
    , "    (because it does not appear in the target language,"
    , "     because it has different subterms in the target language, or"
    , "     because nanopass does not understand the type of one or more of the subterms)"
    , "  * each syntactic category of the source language shared by the target,"
    , "    which allows a pass to override the default translation."
    , "    When no override is needed, these members can be initialized with 'const Nothing'."
    ]
  pure $ TH.DataD [] xlateName tvs Nothing
    [TH.RecC xlateName $ holes ++ overrides]
    []
  where
  xlateName = TH.mkName "Xlate"
  tvs = flip TH.PlainTV () <$> xlateParams x ++ [xlateFParam x]
  holes = flip map (lefts $ xlateProds x) $ \hole ->
    let name = lowerHead hole.syncatName ++ hole.prodName
        t = foldr ArrT hole.holeResult hole.holeArgs
     in (TH.mkName name, noBang, t)
  overrides = flip map (xlateSyncats x) $ \syncat ->
    let name = lowerHead syncat.syncatName
     in (TH.mkName name, noBang, ArrT syncat.fromType syncat.toType)

interpretTypeDesc :: DefdLang -> TypeDesc -> TH.Type
interpretTypeDesc l = go
  where
  go (RecursiveType sName) =
    let syncatCtor = TH.ConT (TH.mkName $ l.langQualPrefix ++ sName)
     in foldl AppT syncatCtor (TH.VarT <$> l.defdLangParams)
  go (VarType vName) = TH.VarT vName
  go (CtorType thName argDescs) = foldl AppT (TH.ConT thName) (go <$> argDescs)
  go (ListType argDesc) = AppT TH.ListT (go argDesc)
  go (NonEmptyType argDesc) = AppT (TH.ConT ''NonEmpty) (go argDesc)
  go (MaybeType argDesc) = AppT (TH.ConT ''Maybe) (go argDesc)
  go (TupleType t1 t2 ts) =
    let tupLen = 2 + length ts
        thTup = TH.TupleT tupLen
        tys = go <$> (t1:t2:ts)
     in foldl AppT thTup tys
  go (MapType kDesc vDesc) = do
    let m = TH.ConT ''Map
        k = go kDesc
        v = go vDesc
     in AppT (AppT m k) v


---------------------------------------
------ Declare Descend Functions ------
---------------------------------------

defineDescend :: DefdLang -> DefdLang -> XlateDef -> Q [Dec]
defineDescend l1 l2 xdef = do
  fmap concat . forM xdef.xlateSyncats $ \XlateSyncatDef{syncatName} -> do
    let funName = TH.mkName $ "descend" ++ syncatName
    TH.addModFinalizer $ TH.putDoc (TH.DeclDoc funName) $ unlines
      [ unwords
        [ "Translate syntax trees starting from"
        , "any t'" ++ l1.langQualPrefix ++ syncatName ++ "' of the t'" ++ show l1.defdLangName ++ "' language"
        , "to the corresponding '" ++ l2.langQualPrefix ++ syncatName ++ "' of the t'" ++ show l2.defdLangName ++ "' language."
        ]
      , ""
      , "Some (hopefully most) of this function was automatically generated by nanopass."
      , unwords
        [ "It is parameterized by an t'Xlate', which"
        , "fills holes for which nanopass could not automatcially determine a translation, and also"
        , "allows for automatic translation to be overridden."
        ]
      ]
    xlateVar <- TH.newName "xlate"
    termVar <- TH.newName "term"
    -- define the automatic case matching
    autoMatches <- case Map.lookup syncatName l1.defdSyncats of
      Nothing -> errorWithoutStackTrace $ "nanopass internal error: failed to find a source syncat that appears as an override: " ++ syncatName
      Just DefdSyncatType{defdProds} -> do
        -- go through all the productions for this syntactic category's type
        forM (Map.toAscList defdProds) $ \(_, prod) -> do
          let pName = TH.nameBase prod.defdProdName
          args <- (TH.newName . TH.nameBase . defdSubtermName) `mapM` prod.defdSubterms
          let pat = TH.ConP prod.defdProdName [] (TH.VarP <$> args)
          let body = case findAuto syncatName pName xdef.xlateProds of
                -- if this production has a hole, call the hole
                Just (Left _) ->
                  let f = TH.mkName $ lowerHead syncatName ++ pName
                      recurse = VarE f `AppE` VarE xlateVar
                   in foldl AppE recurse (VarE <$> args)
                Just (Right auto) ->
                  let e0 = VarE 'pure `AppE` TH.ConE (TH.mkName $ l2.langQualPrefix ++ pName)
                      iAppE a b = TH.InfixE (Just a) (VarE '(<*>)) (Just b)
                      es = zipWith ($) (auto.autoArgs <&> ($ xlateVar)) args
                   in foldl iAppE e0 es
                Nothing -> error "internal nanopass error: found neither hole nor auto"
          pure $ TH.Match pat (TH.NormalB body) []
    let autoBody = TH.CaseE (VarE termVar) autoMatches
    -- define the case match on the result of the override
    termVar' <- TH.newName "term"
    let override = VarE (TH.mkName $ lowerHead syncatName)
                   `AppE` (VarE xlateVar)
                   `AppE` (VarE termVar)
        ovrMatches =
          [ TH.Match (TH.ConP 'Just [] [TH.VarP termVar']) (TH.NormalB $ VarE termVar') []
          , TH.Match (TH.ConP 'Nothing [] []) (TH.NormalB autoBody) []
          ]
    -- tie it all together
    let body = TH.CaseE override ovrMatches
    let clause = TH.Clause [TH.VarP xlateVar, TH.VarP termVar] (TH.NormalB body) []
    -- generate a type signature
    let quantifier = flip TH.PlainTV TH.InferredSpec <$> xdef.xlateParams ++ [xdef.xlateFParam]
        appClass = TH.ConT ''Applicative `AppT` TH.VarT xdef.xlateFParam
        xlateArgTyCon = TH.ConT $ TH.mkName "Xlate"
        xlateArgTy = foldl AppT xlateArgTyCon (TH.VarT <$> xdef.xlateParams ++ [xdef.xlateFParam])
        l1ArgTyCon = TH.ConT $ TH.mkName $ l1.langQualPrefix ++ syncatName
        l1ArgTy = foldl AppT l1ArgTyCon (TH.VarT <$> l1.defdLangParams)
        l2ResTyCon = TH.ConT $ TH.mkName $ l2.langQualPrefix ++ syncatName
        l2ResTyCore = foldl AppT l2ResTyCon (TH.VarT <$> l2.defdLangParams)
        l2ResTy = AppT (TH.VarT xdef.xlateFParam) l2ResTyCore
    -- and emit both signature and definition
    pure
      [ TH.SigD funName $ TH.ForallT quantifier [appClass] $
          xlateArgTy `ArrT` (l1ArgTy `ArrT` l2ResTy)
      , TH.FunD funName [clause]
      ]

---------------------
------ Helpers ------
---------------------

pattern ArrT :: TH.Type -> TH.Type -> TH.Type
pattern ArrT a b = AppT (AppT TH.ArrowT a) b

idiomAppE :: Exp -> Exp -> Exp
idiomAppE a b = TH.InfixE (Just a) (VarE '(<*>)) (Just b)

noBang :: TH.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

containsGrammar :: TypeDesc -> Bool
containsGrammar (RecursiveType _) = True
containsGrammar (VarType _) = False
containsGrammar (CtorType _ ts) = any containsGrammar ts
containsGrammar (ListType t) = containsGrammar t
containsGrammar (MaybeType t) = containsGrammar t
containsGrammar (NonEmptyType t) = containsGrammar t
containsGrammar (TupleType t1 t2 ts) = any containsGrammar (t1:t2:ts)
containsGrammar (MapType t1 t2) = containsGrammar t1 || containsGrammar t2

findAuto :: String -> String -> [XlateProd] -> Maybe XlateProd
findAuto sName pName autosHoles = case filter f autosHoles of
  [] -> Nothing
  x:_ -> Just x
  where
  f :: XlateProd -> Bool
  f (Left x) = x.syncatName == sName && x.prodName == pName
  f (Right x) = x.syncatName == sName && x.prodName == pName


lowerHead :: String -> String
lowerHead [] = []
lowerHead (c:cs) = Char.toLower c : cs

hoistNothing :: Monad m => MaybeT m a
hoistNothing = MaybeT $ pure Nothing
