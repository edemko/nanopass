{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nanopass.Xlate where

import Language.Nanopass.LangDef

import Control.Monad (forM)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Language.Haskell.TH (Q,Dec,Exp)

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
  { xlateParams :: [TH.Name]
  , xlateFParam :: TH.Name
  , xlateHoles :: [XlateHoleDef]
  , xlateOverrides :: [XlateOverrideDef]
  }
data XlateHoleDef = XlateHoleDef
  { grammarName :: String
  , ctorName :: String
  , holeArgs :: [(String, TH.Type)]
  , holeResult :: TH.Type
  }
data XlateOverrideDef = XlateOverrideDef
  { grammarName :: String
  , fromType :: TH.Type
  , toType :: TH.Type
  }

xlateDef :: DefdLang -> DefdLang -> Q XlateDef
xlateDef l1 l2 = do
  let xlateParams = nub (thLangParams l1 ++ thLangParams l2)
  xlateFParam <- if TH.mkName "f" `elem` xlateParams
    then TH.newName "f"
    else pure $ TH.mkName "f"
  let xlateHoles = concatMap (detectHoles xlateFParam l1 l2) $ Map.toAscList (defdGrammars l1)
      xlateOverrides = concatMap (detectOverrides xlateFParam l1 l2) $ Map.toAscList (defdGrammars l1)
  pure $ XlateDef
    { xlateParams
    , xlateFParam
    , xlateHoles
    , xlateOverrides
    }

detectHoles :: TH.Name -> DefdLang -> DefdLang -> (String, DefdGrammarType) -> [XlateHoleDef]
detectHoles fParam l1 l2 (gName, g1) = case Map.lookup gName (defdGrammars l2) of
  Nothing -> [] -- no custom required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just g2 -> concatMap (detectHoleCtors g2) $ Map.toAscList (defdCtors g1)
  where
  detectHoleCtors :: DefdGrammarType -> (String, DefdCtor) -> [XlateHoleDef]
  detectHoleCtors g2 (cName, ctor1) = case Map.lookup cName (defdCtors g2) of
    -- a required hole, because there is no constructor to target
    Nothing -> [createHole cName ctor1]
    Just ctor2
      -- no custom translation required: the arguments of one constructor match up with the arguments of the other
      | (defdSubtermType <$> defdArgs ctor1) == (defdSubtermType <$> defdArgs ctor2) -> []
      -- a required hole, because the arguments of the constructors do not have the same structure
      | otherwise  -> [createHole cName ctor1]
  createHole cName ctor1 =
    let holeArgs = flip map (defdArgs ctor1) $ \DefdSubterm{thSubtermName,defdSubtermType} ->
          (TH.nameBase thSubtermName, interpretTypeDesc l1 defdSubtermType)
        holeCtor = TH.ConT (TH.mkName $ langQualPrefix l2 ++ gName)
        holeCore = foldl TH.AppT holeCtor (TH.VarT <$> thLangParams l2)
        holeResult = TH.AppT (TH.VarT fParam) holeCore
     in XlateHoleDef{grammarName=gName,ctorName=cName,holeArgs,holeResult}

detectOverrides :: TH.Name -> DefdLang -> DefdLang -> (String, DefdGrammarType) -> [XlateOverrideDef]
detectOverrides fParam l1 l2 (gName, _) = case Map.lookup gName (defdGrammars l2) of
  Nothing -> [] -- no translation required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just _ ->
    let fromTypeCtor = TH.ConT (TH.mkName $ langQualPrefix l1 ++ gName)
        fromType = foldl TH.AppT fromTypeCtor (TH.VarT <$> thLangParams l1)
        toTypeCtor = TH.ConT (TH.mkName $ langQualPrefix l2 ++ gName)
        toTypeCore = foldl TH.AppT toTypeCtor (TH.VarT <$> thLangParams l2)
        toType = TH.AppT (TH.ConT ''Maybe) $ TH.AppT (TH.VarT fParam) toTypeCore
     in [XlateOverrideDef{grammarName = gName,fromType,toType}]

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
  holes = flip map (xlateHoles x) $ \XlateHoleDef{grammarName,ctorName,holeArgs,holeResult} ->
    let name = lowerHead grammarName ++ ctorName
        t = foldr (\(_, a) r -> TH.AppT (TH.AppT TH.ArrowT a) r) holeResult holeArgs
     in (TH.mkName name, noBang, t)
  overrides = flip map (xlateOverrides x) $ \XlateOverrideDef{grammarName,fromType,toType} -> 
    let name = lowerHead grammarName
        t = TH.AppT (TH.AppT TH.ArrowT fromType) toType
     in (TH.mkName name, noBang, t)

interpretTypeDesc :: DefdLang -> TypeDesc -> TH.Type
interpretTypeDesc DefdLang{langQualPrefix,thLangParams} = go
  where
  go (GrammarType gName) =
    let grammarCtor = TH.ConT (TH.mkName $ langQualPrefix ++ gName)
     in foldl TH.AppT grammarCtor (TH.VarT <$> thLangParams)
  go (VarType vName) = TH.VarT vName
  go (CtorType thName argDescs) = foldl TH.AppT (TH.ConT thName) (go <$> argDescs)
  go (ListType argDesc) = TH.AppT TH.ListT (go argDesc)
  go (NonEmptyType argDesc) = TH.AppT (TH.ConT ''NonEmpty) (go argDesc)
  go (MaybeType argDesc) = TH.AppT (TH.ConT ''Maybe) (go argDesc)
  go (TupleType t1 t2 ts) =
    let tupLen = 2 + length ts
        thTup = TH.TupleT tupLen
        tys = go <$> (t1:t2:ts)
     in foldl TH.AppT thTup tys
  go (AlistType kDesc vDesc) =
    let k = go kDesc
        v = go vDesc
     in TH.AppT TH.ListT $ TH.AppT (TH.AppT (TH.TupleT 2) k) v
  go (MapType kDesc vDesc) = do
    let m = TH.ConT ''Map
        k = go kDesc
        v = go vDesc
     in TH.AppT (TH.AppT m k) v


---------------------------------------
------ Declare Descend Functions ------
---------------------------------------

defineDescendA :: DefdLang -> DefdLang -> XlateDef -> Q [Dec]
defineDescendA l1 l2 XlateDef{xlateParams,xlateFParam,xlateOverrides,xlateHoles} = do
  fmap concat . forM xlateOverrides $ \XlateOverrideDef{grammarName} -> do
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
          let body = case findHole grammarName cName xlateHoles of
                -- if this constructor has a hole, call the hole
                Just _ ->
                  let f = TH.mkName $ lowerHead grammarName ++ cName
                      recurse = TH.VarE f `TH.AppE` TH.VarE xlateVar
                   in foldl TH.AppE recurse (TH.VarE <$> args)
                -- TODO otherwise, generate a catamorphism
                Nothing -> 
                  let e0 = TH.VarE 'pure `TH.AppE` TH.ConE (TH.mkName $ langQualPrefix l2 ++ cName)
                      iAppE a b = TH.InfixE (Just a) (TH.VarE '(<*>)) (Just b)
                      es = zipWith (descendCata xlateVar) args (defdSubtermType <$> defdArgs)
                   in foldl iAppE e0 es
                -- TH.VarE (TH.mkName "undefined")
          pure $ TH.Match pat (TH.NormalB body) []
    let autoBody = TH.CaseE (TH.VarE termVar) autoMatches
    -- define the case match on the result of the override
    termVar' <- TH.newName "term"
    let override = TH.VarE (TH.mkName $ lowerHead grammarName)
                   `TH.AppE` (TH.VarE xlateVar)
                   `TH.AppE` (TH.VarE termVar)
        ovrMatches =
          [ TH.Match (TH.ConP 'Just [TH.VarP termVar']) (TH.NormalB $ TH.VarE termVar') []
          , TH.Match (TH.ConP 'Nothing []) (TH.NormalB autoBody) []
          ]
    -- tie it all together
    let body = TH.CaseE override ovrMatches
    let clause = TH.Clause [TH.VarP xlateVar, TH.VarP termVar] (TH.NormalB body) []
    -- generate a type signature
    let quantifier = TH.PlainTV <$> xlateParams ++ [xlateFParam]
        appClass = TH.ConT ''Applicative `TH.AppT` TH.VarT xlateFParam
        xlateArgTyCon = TH.ConT $ TH.mkName "XlateA"
        xlateArgTy = foldl TH.AppT xlateArgTyCon (TH.VarT <$> xlateParams ++ [xlateFParam])
        l1ArgTyCon = TH.ConT $ TH.mkName $ langQualPrefix l1 ++ grammarName
        l1ArgTy = foldl TH.AppT l1ArgTyCon (TH.VarT <$> thLangParams l1)
        l2ResTyCon = TH.ConT $ TH.mkName $ langQualPrefix l2 ++ grammarName
        l2ResTyCore = foldl TH.AppT l2ResTyCon (TH.VarT <$> thLangParams l2)
        l2ResTy = TH.AppT (TH.VarT xlateFParam) l2ResTyCore
        funT a b = TH.AppT (TH.AppT TH.ArrowT a) b
        sigType = xlateArgTy `funT` (l1ArgTy `funT` l2ResTy)
    -- and emit both signature and definition
    pure
      [ TH.SigD funName $ TH.ForallT quantifier [appClass] sigType
      , TH.FunD funName [clause]
      ]

descendCata :: TH.Name -> TH.Name -> TypeDesc -> Exp
descendCata _ argVar t0
  | not (containsGrammar t0) = TH.VarE 'pure `TH.AppE` TH.VarE argVar
descendCata xlateVar argVar (GrammarType gName) =
  let recName = TH.mkName $ "descend" ++ gName ++ "A"
   in TH.VarE recName `TH.AppE` TH.VarE xlateVar `TH.AppE` TH.VarE argVar

---------------------
------ Helpers ------
---------------------

containsGrammar :: TypeDesc -> Bool
containsGrammar (GrammarType _) = True
containsGrammar (VarType _) = False
containsGrammar (CtorType _ ts) = any containsGrammar ts
containsGrammar (ListType t) = containsGrammar t
containsGrammar (MaybeType t) = containsGrammar t
containsGrammar (NonEmptyType t) = containsGrammar t
containsGrammar (TupleType t1 t2 ts) = any containsGrammar (t1:t2:ts)
containsGrammar (AlistType t1 t2) = containsGrammar t1 || containsGrammar t2
containsGrammar (MapType t1 t2) = containsGrammar t1 || containsGrammar t2

findHole :: String -> String -> [XlateHoleDef] -> Maybe XlateHoleDef
findHole gName cName holes = case filter f holes of
  [] -> Nothing
  x:_ -> Just x
  where
  f XlateHoleDef{grammarName,ctorName} = grammarName == gName && ctorName == cName

lowerHead :: String -> String
lowerHead [] = []
lowerHead (c:cs) = Char.toLower c : cs