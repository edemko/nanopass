{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nanopass.Xlate where

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


mkXlateA :: DefdLang -> DefdLang -> Q [Dec]
mkXlateA l1 l2 = xlateDef l1 l2 >>= declareXlate l1 l2

declareXlate :: DefdLang -> DefdLang -> XlateDef -> Q [Dec]
declareXlate l1 l2 xlate = do
  descendsA <- defineDescendA l1 l2 xlate
  pure $ declareTypeAp xlate : descendsA

---------------------------------------------
------ Gather Translation Requirements ------
---------------------------------------------

data XlateDef = XlateDef
  { xlateParams :: [TH.Name] -- ^ the type parameters of both languages, merged
  , xlateFParam :: TH.Name -- ^ a type for an Applicative parameter
  , xlateGrammars :: [XlateGrammarDef]
    -- ^ information about the grammars shared by both source and target
    -- this is used to allow users to override the bahavior of automatic translation
  , xlateCtors :: [Either XlateHoleDef XlateAuto]
    -- ^ information about the type constructors in teh source that are missing in the target
    -- this is so that we require the user to supply these in an Xlate type
  }
data XlateAuto = XlateAuto
  { grammarName :: String
  , ctorName :: String
  , autoArgs :: [TH.Name -> TH.Name -> Exp] -- functions from xlate and subterm variables to auto-translator
  }
data XlateHoleDef = XlateHoleDef
  { grammarName :: String -- the name of the grammar shared by source and target
  , ctorName :: String -- the name of the source grammar's constructor
  , holeArgs :: [TH.Type] -- the types of the arguments to source constructor
  , holeResult :: TH.Type -- the type of the target grammar that must be supplied
  }
data XlateGrammarDef = XlateGrammarDef
  { grammarName :: String -- the name of the grammar shared by source and target
  , fromType :: TH.Type -- parameterized type of the source language at this grammar
  , toType :: TH.Type -- parameterized type of the target language at this grammar
  }

xlateDef :: DefdLang -> DefdLang -> Q XlateDef
xlateDef l1 l2 = do
  let xlateParams = nub (thLangParams l1 ++ thLangParams l2)
  xlateFParam <- if TH.mkName "f" `elem` xlateParams
    then TH.newName "f"
    else pure $ TH.mkName "f"
  xlateCtors <- fmap concat $ forM (Map.toAscList $ defdGrammars l1) $ detectHoles xlateFParam l1 l2
  let xlateGrammars = concatMap (detectOverrides xlateFParam l1 l2) $ Map.toAscList (defdGrammars l1)
  pure $ XlateDef
    { xlateParams
    , xlateFParam
    , xlateGrammars
    , xlateCtors
    }

detectHoles :: TH.Name -> DefdLang -> DefdLang -> (String, DefdGrammarType) -> Q [Either XlateHoleDef XlateAuto]
detectHoles fParam l1 l2 (gName, g1) = case Map.lookup gName (defdGrammars l2) of
  Nothing -> pure [] -- no translation required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just g2 -> fmap concat $ forM (Map.toAscList $ defdCtors g1) $ detectHoleCtors g2
  where
  detectHoleCtors :: DefdGrammarType -> (String, DefdCtor) -> Q [Either XlateHoleDef XlateAuto]
  detectHoleCtors g2 (cName, ctor1) = case Map.lookup cName (defdCtors g2) of
    -- a required hole, because there is no constructor to target
    Nothing -> pure [Left $ createHole cName ctor1]
    Just ctor2
      -- no custom translation required: the arguments of one constructor match up with the arguments of the other
      | tys1 <- (defdSubtermType <$> defdArgs ctor1)
      , tys2 <- (defdSubtermType <$> defdArgs ctor2)
      , tys1 == tys2 -> runMaybeT (createAuto `mapM` tys1) >>= \case
          Nothing -> pure [Left $ createHole cName ctor1] -- a required hole because no auto-translation possible
          Just autoArgs -> do
            pure [Right XlateAuto{grammarName=gName,ctorName=cName,autoArgs}]
      -- a required hole, because the arguments of the constructors do not have the same structure
      | otherwise  -> pure [Left $ createHole cName ctor1]
  createHole cName ctor1 =
    let holeArgs = flip map (defdArgs ctor1) $ \DefdSubterm{defdSubtermType} ->
          interpretTypeDesc l1 defdSubtermType
        holeCtor = TH.ConT (TH.mkName $ langQualPrefix l2 ++ gName)
        holeCore = foldl AppT holeCtor (TH.VarT <$> thLangParams l2)
        holeResult = AppT (TH.VarT fParam) holeCore
     in XlateHoleDef{grammarName=gName,ctorName=cName,holeArgs,holeResult}

detectOverrides :: TH.Name -> DefdLang -> DefdLang -> (String, DefdGrammarType) -> [XlateGrammarDef]
detectOverrides fParam l1 l2 (gName, _) = case Map.lookup gName (defdGrammars l2) of
  Nothing -> [] -- no translation required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just _ ->
    let fromTypeCtor = TH.ConT (TH.mkName $ langQualPrefix l1 ++ gName)
        fromType = foldl AppT fromTypeCtor (TH.VarT <$> thLangParams l1)
        toTypeCtor = TH.ConT (TH.mkName $ langQualPrefix l2 ++ gName)
        toTypeCore = foldl AppT toTypeCtor (TH.VarT <$> thLangParams l2)
        toType = AppT (TH.ConT ''Maybe) $ AppT (TH.VarT fParam) toTypeCore
     in [XlateGrammarDef{grammarName = gName,fromType,toType}]

createAuto :: TypeDesc -> MaybeT Q (TH.Name -> TH.Name -> Exp)
createAuto t0 | not (containsGrammar t0) = do
  let auto _ argVar = VarE 'pure `AppE` VarE argVar
  pure auto
createAuto (GrammarType gName) = do
  let recName = TH.mkName $ "descend" ++ gName ++ "A"
      auto xlateVar argVar = VarE recName `AppE` VarE xlateVar `AppE` VarE argVar
  pure auto
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


-- pure (\(t1, t2) -> (t1, t2))
--  <*> (descendExprA xlate) a1
--  <*> (descendExprA xlate) a2

---------------------------------
------ Declare XLate Types ------
---------------------------------

declareTypeAp :: XlateDef -> Dec
declareTypeAp x =
  TH.DataD [] xlateName tvs Nothing
    [TH.RecC xlateName $ holes ++ overrides]
    []
  where
  xlateName = TH.mkName "XlateA"
  tvs = TH.PlainTV <$> xlateParams x ++ [xlateFParam x]
  holes = flip map (lefts $ xlateCtors x) $ \XlateHoleDef{grammarName,ctorName,holeArgs,holeResult} ->
    let name = lowerHead grammarName ++ ctorName
        t = foldr ArrT holeResult holeArgs
     in (TH.mkName name, noBang, t)
  overrides = flip map (xlateGrammars x) $ \XlateGrammarDef{grammarName,fromType,toType} ->
    let name = lowerHead grammarName
     in (TH.mkName name, noBang, ArrT fromType toType)

interpretTypeDesc :: DefdLang -> TypeDesc -> TH.Type
interpretTypeDesc DefdLang{langQualPrefix,thLangParams} = go
  where
  go (GrammarType gName) =
    let grammarCtor = TH.ConT (TH.mkName $ langQualPrefix ++ gName)
     in foldl AppT grammarCtor (TH.VarT <$> thLangParams)
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

defineDescendA :: DefdLang -> DefdLang -> XlateDef -> Q [Dec]
defineDescendA l1 l2 XlateDef{xlateParams,xlateFParam,xlateGrammars,xlateCtors} = do
  fmap concat . forM xlateGrammars $ \XlateGrammarDef{grammarName} -> do
    let funName = TH.mkName $ "descend" ++ grammarName ++ "A"
    xlateVar <- TH.newName "xlate"
    termVar <- TH.newName "term"
    -- define the automatic case matching
    autoMatches <- case Map.lookup grammarName (defdGrammars l1) of
      Nothing -> errorWithoutStackTrace $ "nanopass internal error: failed to find a source grammar that appears as an override: " ++ grammarName
      Just DefdGrammarType{defdCtors} -> do
        -- go through all the constructors for this grammar type
        forM (Map.toAscList defdCtors) $ \(_, DefdCtor{thTermName,defdArgs}) -> do
          let cName = TH.nameBase thTermName
          args <- (TH.newName . TH.nameBase . thSubtermName) `mapM` defdArgs
          let pat = TH.ConP thTermName (TH.VarP <$> args)
          let body = case findAuto grammarName cName xlateCtors of
                -- if this constructor has a hole, call the hole
                Just (Left _) ->
                  let f = TH.mkName $ lowerHead grammarName ++ cName
                      recurse = VarE f `AppE` VarE xlateVar
                   in foldl AppE recurse (VarE <$> args)
                -- TODO otherwise, generate a catamorphism
                Just (Right XlateAuto{autoArgs}) ->
                  let e0 = VarE 'pure `AppE` TH.ConE (TH.mkName $ langQualPrefix l2 ++ cName)
                      iAppE a b = TH.InfixE (Just a) (VarE '(<*>)) (Just b)
                      es = zipWith ($) (autoArgs <&> ($ xlateVar)) args
                   in foldl iAppE e0 es
                Nothing -> error "internal nanopass error: found neither hold nor auto"
          pure $ TH.Match pat (TH.NormalB body) []
    let autoBody = TH.CaseE (VarE termVar) autoMatches
    -- define the case match on the result of the override
    termVar' <- TH.newName "term"
    let override = VarE (TH.mkName $ lowerHead grammarName)
                   `AppE` (VarE xlateVar)
                   `AppE` (VarE termVar)
        ovrMatches =
          [ TH.Match (TH.ConP 'Just [TH.VarP termVar']) (TH.NormalB $ VarE termVar') []
          , TH.Match (TH.ConP 'Nothing []) (TH.NormalB autoBody) []
          ]
    -- tie it all together
    let body = TH.CaseE override ovrMatches
    let clause = TH.Clause [TH.VarP xlateVar, TH.VarP termVar] (TH.NormalB body) []
    -- generate a type signature
    let quantifier = TH.PlainTV <$> xlateParams ++ [xlateFParam]
        appClass = TH.ConT ''Applicative `AppT` TH.VarT xlateFParam
        xlateArgTyCon = TH.ConT $ TH.mkName "XlateA"
        xlateArgTy = foldl AppT xlateArgTyCon (TH.VarT <$> xlateParams ++ [xlateFParam])
        l1ArgTyCon = TH.ConT $ TH.mkName $ langQualPrefix l1 ++ grammarName
        l1ArgTy = foldl AppT l1ArgTyCon (TH.VarT <$> thLangParams l1)
        l2ResTyCon = TH.ConT $ TH.mkName $ langQualPrefix l2 ++ grammarName
        l2ResTyCore = foldl AppT l2ResTyCon (TH.VarT <$> thLangParams l2)
        l2ResTy = AppT (TH.VarT xlateFParam) l2ResTyCore
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

containsGrammar :: TypeDesc -> Bool
containsGrammar (GrammarType _) = True
containsGrammar (VarType _) = False
containsGrammar (CtorType _ ts) = any containsGrammar ts
containsGrammar (ListType t) = containsGrammar t
containsGrammar (MaybeType t) = containsGrammar t
containsGrammar (NonEmptyType t) = containsGrammar t
containsGrammar (TupleType t1 t2 ts) = any containsGrammar (t1:t2:ts)
containsGrammar (MapType t1 t2) = containsGrammar t1 || containsGrammar t2

findAuto :: String -> String -> [Either XlateHoleDef XlateAuto] -> Maybe (Either XlateHoleDef XlateAuto)
findAuto gName cName autosHoles = case filter f autosHoles of
  [] -> Nothing
  x:_ -> Just x
  where
  f (Left XlateHoleDef{grammarName,ctorName}) = grammarName == gName && ctorName == cName
  f (Right XlateAuto{grammarName,ctorName}) = grammarName == gName && ctorName == cName


lowerHead :: String -> String
lowerHead [] = []
lowerHead (c:cs) = Char.toLower c : cs

nothing :: Monad m => MaybeT m a
nothing = MaybeT $ pure Nothing