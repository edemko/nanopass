{-# LANGUAGE DataKinds #-}
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
  , XlateNontermDef(..)
  ) where

import Nanopass.Internal.Representation

import Control.Monad (forM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Either (lefts)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH (Exp(AppE,VarE))
import Language.Haskell.TH (Q,Dec)
import Language.Haskell.TH (Type(AppT))

import qualified Control.Monad.Trans as M
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH


mkXlate :: Language 'Valid UpDotName -> Language 'Valid UpDotName -> Q [Dec]
mkXlate l1 l2 = xlateDef l1 l2 >>= declareXlate l1 l2

declareXlate :: Language 'Valid UpDotName -> Language 'Valid UpDotName -> XlateDef -> Q [Dec]
declareXlate l1 l2 xlate = do
  xlateType <- declareType xlate
  xlateTypeI <- declareTypeI xlate
  xlateLifter <- declareXlateLifter xlate
  descends <- defineDescend l1 l2 xlate
  pure $ xlateType : xlateTypeI : xlateLifter ++ descends

---------------------------------------------
------ Gather Translation Requirements ------
---------------------------------------------

data XlateDef = XlateDef
  { xlateParams :: [TH.Name] -- ^ the type parameters of both languages, merged
  , xlateFParam :: TH.Name -- ^ a type for an Applicative parameter
  , xlateNonterms :: [XlateNontermDef]
    -- ^ information about the syntactic cateories shared by both source and target
    -- this is used to allow users to override the bahavior of automatic translation
  , xlateProds :: [XlateProd] -- FIXME these should go under xlateNonterms, probly
    -- ^ information about the productions in the source that are missing in the target
    -- this is so that we require the user to supply these in an Xlate type
  , xlateFrom :: Language 'Valid UpDotName
  , xlateTo :: Language 'Valid UpDotName
  }
type XlateProd = Either XlateHoleDef XlateAuto
data XlateAuto = XlateAuto
  { nontermName :: UpName
  , prodName :: UpName
  , autoArgs :: [TH.Name -> TH.Name -> Exp] -- functions from xlate and subterm variables to auto-translator
  }
data XlateHoleDef = XlateHoleDef
  { nontermName :: UpName -- the name of the syntactic category shared by source and target
  , prodName :: UpName -- the name of the source production
  , holeArgs :: [TH.Type] -- the types of the subterms of the source production
  , holeResult :: TH.Type -- the type of the target syntactic category that must be supplied
  }
data XlateNontermDef = XlateNontermDef
  { nontermName :: UpName -- the name of the syntactic category shared by source and target
  , fromType :: TH.Type -- parameterized type of the source language at this syntactic category
  , toType :: TH.Type -- parameterized type of the target language at this syntactic category
  }

xlateDef :: Language 'Valid UpDotName -> Language 'Valid UpDotName -> Q XlateDef
xlateDef l1 l2 = do
  let xlateParams = (.th) <$> nub (l1.langInfo.langParams ++ l2.langInfo.langParams)
  xlateFParam <- if TH.mkName "f" `elem` xlateParams
    then TH.newName "f"
    else pure $ TH.mkName "f"
  xlateProds <- fmap concat $ forM (Map.toAscList $ l1.langInfo.nonterms) $ detectHoles l1 l2
  let xlateNonterms = concatMap (detectOverrides l1 l2) $ Map.toAscList l1.langInfo.nonterms
  pure $ XlateDef
    { xlateParams
    , xlateFParam
    , xlateNonterms
    , xlateProds
    , xlateFrom = l1
    , xlateTo = l2
    }

detectHoles :: Language 'Valid UpDotName
            -> Language 'Valid UpDotName
            -> (UpName, Nonterm 'Valid)
            -> Q [Either XlateHoleDef XlateAuto]
detectHoles l1 l2 (sName, s1) = case Map.lookup sName l2.langInfo.nonterms of
  Nothing -> pure [] -- no translation required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just s2 -> fmap concat $ forM (Map.toAscList s1.productions) $ detectHoleCtors s2
  where
  detectHoleCtors :: Nonterm 'Valid -> (UpName, Production 'Valid) -> Q [Either XlateHoleDef XlateAuto]
  detectHoleCtors s2 (pName, prod1) = case Map.lookup pName s2.productions of
    -- a required hole, because there is no constructor to target
    Nothing -> pure [Left $ createHole pName prod1]
    Just prod2
      -- no custom translation required: the arguments of one constructor match up with the arguments of the other
      | tys1 <- prod1.subterms
      , tys2 <- prod2.subterms
      , tys1 == tys2 -> runMaybeT (createAuto `mapM` tys1) >>= \case
          Nothing -> pure [Left $ createHole pName prod1] -- a required hole because no auto-translation possible
          Just autoArgs -> do
            pure [Right XlateAuto{nontermName=sName,prodName=pName,autoArgs}]
      -- a required hole, because the arguments of the constructors do not have the same structure
      | otherwise  -> pure [Left $ createHole pName prod1]
  createHole :: UpName -> Production 'Valid -> XlateHoleDef
  createHole pName prod1 =
    let holeArgs = flip map prod1.subterms $ \subterm ->
          interpretTypeDesc l1 subterm
        holeCtor = TH.ConT (TH.mkName $ fromUpDotName (upDotChBase l2.langName.name sName))
        holeResult = foldl AppT holeCtor (TH.VarT . (.th) <$> l2.langInfo.langParams)
     in XlateHoleDef{nontermName=sName,prodName=pName,holeArgs,holeResult}

detectOverrides :: Language 'Valid UpDotName -> Language 'Valid UpDotName -> (UpName, Nonterm 'Valid) -> [XlateNontermDef]
detectOverrides l1 l2 (sName, _) = case Map.lookup sName l2.langInfo.nonterms of
  Nothing -> [] -- no translation required: no l2 ctor can use the a type corresponding to this l1 type (because it doesn't exist)
  Just _ ->
    let fromTypeCtor = TH.ConT (TH.mkName $ fromUpDotName (upDotChBase l1.langName.name sName))
        fromType = foldl AppT fromTypeCtor (TH.VarT . (.th) <$> l1.langInfo.langParams)
        toTypeCtor = TH.ConT (TH.mkName $ fromUpDotName (upDotChBase l2.langName.name sName))
        toType = foldl AppT toTypeCtor (TH.VarT . (.th) <$> l2.langInfo.langParams)
     in [XlateNontermDef{nontermName = sName,fromType,toType}]

createAuto :: TypeDesc 'Valid -> MaybeT Q (TH.Name -> TH.Name -> Exp)
createAuto (RecursiveType sName) = do
  let repName = TH.mkName $ "descend" ++ fromUpName sName
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
      let travCandidate = foldl AppT (TH.ConT tyName.th) (interpretTypeDesc (Language undefined undefined) <$> ts')
      isTraversable <- M.lift $ TH.isInstance ''Traversable [travCandidate]
      if isTraversable then traversableAuto t else hoistNothing
  -- TODO maybe try Bitraversable
  | otherwise = hoistNothing
createAuto (ListType t) = traversableAuto t
createAuto (MaybeType t) = traversableAuto t
createAuto (NonEmptyType t) = traversableAuto t
createAuto UnitType = do
  let auto _ _ = VarE 'pure `AppE` TH.TupE []
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

traversableAuto :: TypeDesc 'Valid -> MaybeT Q (TH.Name -> TH.Name -> Exp)
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
    [ "This type is used to parameterize the nanopass-generated translation functions @descend\\<Syntactic Category\\>@."
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
  holes <- forM (lefts $ xlateProds x) $ \hole -> do
    let name = TH.mkName $ "on" ++ fromUpName hole.nontermName ++ fromUpName hole.prodName
        r = TH.VarT x.xlateFParam `AppT` hole.holeResult
        t = foldr ArrT r hole.holeArgs
    TH.addModFinalizer $ TH.putDoc (TH.DeclDoc name) $ unlines
      [ "No automatic translation for"
      , concat
        [ "the v'", fromUpDotName (upDotChBase x.xlateFrom.langName.name hole.prodName), "' production "
        , "of t'", fromUpDotName (upDotChBase x.xlateFrom.langName.name hole.nontermName), "'"
        ]
      , "could be generated by Nanopass."
      ]
    pure (name, bang, t)
  overrides <- forM x.xlateNonterms $ \nonterm -> do
    let name = TH.mkName $ "on" ++ fromUpName nonterm.nontermName
        r = TH.ConT ''Maybe `AppT` (TH.VarT x.xlateFParam `AppT` nonterm.toType)
    TH.addModFinalizer $ TH.putDoc (TH.DeclDoc name) $ unlines
      [ "This member allows you to override the default translation for"
      , unwords
        [ "The", "t'" ++ fromUpDotName (upDotChBase x.xlateFrom.langName.name nonterm.nontermName) ++ "'"
        , "syntactic category."
        ]
      , "Produce a 'Just' value to override the automatic translation."
      , "If no overrides are needed, use @'const' 'Nothing'@."
      ]
    pure (name, bang, ArrT nonterm.fromType r)
  pure $ TH.DataD [] xlateName tvs Nothing
    [TH.RecC xlateName $ holes ++ overrides]
    []
  where
  xlateName = TH.mkName "Xlate"
  tvs = flip TH.PlainTV () <$> xlateParams x ++ [xlateFParam x]

declareTypeI :: XlateDef -> Q Dec
declareTypeI x = do
  TH.addModFinalizer $ TH.putDoc (TH.DeclDoc xlateName) $ unlines
    [ "This type is used to parameterize the nanopass-generated translation functions @descend*I@."
    , "It is the pure (i.e. does not require an 'Applicative') version of 'Xlate'."
    , ""
    , "See 'Xlate' for more detail."
    ]
  holes <- forM (lefts x.xlateProds) $ \hole -> do
    let name = TH.mkName $ "on" ++ fromUpName hole.nontermName ++ fromUpName hole.prodName ++ "I"
        t = foldr ArrT hole.holeResult hole.holeArgs
    TH.addModFinalizer $ TH.putDoc (TH.DeclDoc name) $ unlines
      [ "No automatic translation for"
      , concat
        [ "the v'", fromUpDotName (upDotChBase x.xlateFrom.langName.name hole.prodName), "' production "
        , "of t'", fromUpDotName (upDotChBase x.xlateFrom.langName.name hole.nontermName), "'"
        ]
      , "could be generated by Nanopass."
      ]
    pure (name, bang, t)
  overrides <- forM x.xlateNonterms $ \nonterm -> do
    let name = TH.mkName $ "on" ++ fromUpName nonterm.nontermName ++ "I"
        r = TH.ConT ''Maybe `AppT` nonterm.toType
    TH.addModFinalizer $ TH.putDoc (TH.DeclDoc name) $ unlines
      [ "This member allows you to override the default translation for"
      , unwords
        [ "The", "t'" ++ fromUpDotName (upDotChBase x.xlateFrom.langName.name nonterm.nontermName) ++ "'"
        , "syntactic category."
        ]
      , "Produce a 'Just' value to override the automatic translation."
      , "If no overrides are needed, use @'const' 'Nothing'@."
      ]
    pure (name, bang, ArrT nonterm.fromType r)
  pure $ TH.DataD [] xlateName tvs Nothing
    [TH.RecC xlateName $ holes ++ overrides]
    []
  where
  xlateName = TH.mkName "XlateI"
  tvs = flip TH.PlainTV () <$> xlateParams x

declareXlateLifter :: XlateDef -> Q [Dec]
declareXlateLifter x = do
  let liftName = TH.mkName "idXlate"
  TH.addModFinalizer $ TH.putDoc (TH.DeclDoc liftName) $ unlines
    [ "This function is used by Nanopass to implement the @descend\\<Syntactic Category\\>I@ functions."
    , "It is used only to lift a pure 'XlateI' parameter into an 'Xlate'."
    , "This way, pure translations can use the same code paths as the more general 'Control.Applicative.Applicative' translations."
    , "Internally, it just arranges wrapping and unwrapping of t'Data.Functor.Identity.Identity', which are no-ops."
    ]
  let quantifier = flip TH.PlainTV TH.InferredSpec <$> x.xlateParams
      xlateApTyCon = TH.ConT $ TH.mkName "Xlate"
      xlateApTy = foldl AppT xlateApTyCon ((TH.VarT <$> x.xlateParams) ++ [TH.ConT ''Identity])
      xlateIdTyCon = TH.ConT $ TH.mkName "XlateI"
      xlateIdTy = foldl AppT xlateIdTyCon (TH.VarT <$> x.xlateParams)
  xlateVar <- TH.newName "xlate"
  holeMembers <- holes xlateVar
  ovrMembers <- overrides xlateVar
  let body = TH.RecConE (TH.mkName "Xlate") (holeMembers ++ ovrMembers)
      clause = TH.Clause [TH.VarP xlateVar] (TH.NormalB body) []
  pure
    [ TH.SigD liftName $ TH.ForallT quantifier [] $
        xlateIdTy `ArrT` xlateApTy
    , TH.FunD liftName [clause]
    ]
  where
  holes xlateVar = forM (lefts x.xlateProds) $ \hole -> do
    let nameAp = TH.mkName $ "on" ++ fromUpName hole.nontermName ++ fromUpName hole.prodName
        nameId = TH.mkName $ "on" ++ fromUpName hole.nontermName ++ fromUpName hole.prodName ++ "I"
    subtermNames <- forM hole.holeArgs $ \_ -> do
      TH.newName "subterm"
    let lam = TH.LamE (TH.VarP <$> subtermNames) body
        body = TH.ConE 'Identity `AppE` foldl AppE delegate (TH.VarE <$> subtermNames)
        delegate = TH.VarE nameId `AppE` TH.VarE xlateVar
    pure (nameAp, lam)
  overrides xlateVar = forM x.xlateNonterms $ \nonterm -> do
    let nameAp = TH.mkName $ "on" ++ fromUpName nonterm.nontermName
        nameId = TH.mkName $ "on" ++ fromUpName nonterm.nontermName ++ "I"
    varName <- TH.newName "term0"
    let lam = TH.LamE [TH.VarP varName] body
        body = TH.InfixE (Just $ TH.ConE 'Identity) (TH.VarE '(<$>)) (Just delegate)
        delegate = (TH.VarE nameId `AppE` TH.VarE xlateVar) `AppE` TH.VarE varName
    pure (nameAp, lam)

interpretTypeDesc :: Language 'Valid UpDotName -> TypeDesc 'Valid -> TH.Type
interpretTypeDesc l = go
  where
  go (RecursiveType sName) =
    let nontermCtor = TH.ConT (TH.mkName . fromUpDotName $ upDotChBase l.langName.name sName)
     in foldl AppT nontermCtor (TH.VarT . (.th) <$> l.langInfo.langParams)
  go (VarType vName) = TH.VarT vName.th
  go (CtorType thName argDescs) = foldl AppT (TH.ConT thName.th) (go <$> argDescs)
  go (ListType argDesc) = AppT TH.ListT (go argDesc)
  go (NonEmptyType argDesc) = AppT (TH.ConT ''NonEmpty) (go argDesc)
  go (MaybeType argDesc) = AppT (TH.ConT ''Maybe) (go argDesc)
  go UnitType = TH.TupleT 0
  go (TupleType t1 t2 ts) =
    let tupLen = 2 + length ts
        thTup = TH.TupleT tupLen
        tys = go <$> (t1:t2:ts)
     in foldl AppT thTup tys

---------------------------------------
------ Declare Descend Functions ------
---------------------------------------

defineDescend :: Language 'Valid UpDotName -> Language 'Valid UpDotName -> XlateDef -> Q [Dec]
defineDescend l1 l2 xdef = do
  fmap concat . forM xdef.xlateNonterms $ \XlateNontermDef{nontermName} -> do
    let funName = TH.mkName $ "descend" ++ fromUpName nontermName
        funNameId = TH.mkName $ "descend" ++ fromUpName nontermName ++ "I"
    TH.addModFinalizer $ TH.putDoc (TH.DeclDoc funName) $ unlines
      [ unwords
        [ "Translate syntax trees starting from"
        , "any t'" ++ fromUpDotName (upDotChBase l1.langName.name nontermName) ++ "' of the t'" ++ show l1.langName.th ++ "' language"
        , "to the corresponding '" ++ fromUpDotName (upDotChBase l2.langName.name nontermName) ++ "' of the t'" ++ show l2.langName.th ++ "' language."
        ]
      , ""
      , "Some (hopefully most) of this function was automatically generated by nanopass."
      , unwords
        [ "It is parameterized by an t'Xlate', which"
        , "fills holes for which nanopass could not automatcially determine a translation, and also"
        , "allows for automatic translation to be overridden."
        ]
      ]
    TH.addModFinalizer $ TH.putDoc (TH.DeclDoc funNameId) $ unlines
      [ unwords
        [ "Translate syntax trees starting from"
        , "any t'" ++ fromUpDotName (upDotChBase l1.langName.name nontermName) ++ "' of the t'" ++ show l1.langName.th ++ "' language"
        , "to the corresponding '" ++ fromUpDotName (upDotChBase l2.langName.name nontermName) ++ "' of the t'" ++ show l2.langName.th ++ "' language."
        ]
      , ""
      , "This is the pure (i.e. no 'Applicative' required) version of '"++show funName++"'."
      , "This version is parameterized by an t'XlateI' rather than an t'Xlate'."
      , "See '"++show funName++"' for more details."
      ]
    xlateVar <- TH.newName "xlate"
    termVar <- TH.newName "term"
    -- define the automatic case matching
    autoMatches <- case Map.lookup nontermName l1.langInfo.nonterms of
      Nothing -> errorWithoutStackTrace $ "nanopass internal error: failed to find a source nonterm that appears as an override: " ++ fromUpName nontermName
      Just Nonterm{productions} -> do
        -- go through all the productions for this syntactic category's type
        forM (Map.toAscList productions) $ \(_, prod) -> do
          args <- TH.newName `mapM` take (length prod.subterms) base26
          let pat = TH.ConP prod.prodName.th [] (TH.VarP <$> args)
          let body = case findAuto nontermName prod.prodName.name xdef.xlateProds of
                -- if this production has a hole, call the hole
                Just (Left _) ->
                  let f = TH.mkName $ "on" ++ fromUpName nontermName ++ fromUpName prod.prodName.name
                      recurse = VarE f `AppE` VarE xlateVar
                   in foldl AppE recurse (VarE <$> args)
                Just (Right auto) ->
                  let e0 = VarE 'pure `AppE` TH.ConE (TH.mkName . fromUpDotName $ upDotChBase l2.langName.name prod.prodName.name)
                      iAppE a b = TH.InfixE (Just a) (VarE '(<*>)) (Just b)
                      es = zipWith ($) (auto.autoArgs <&> ($ xlateVar)) args
                   in foldl iAppE e0 es
                Nothing -> error "internal nanopass error: found neither hole nor auto"
          pure $ TH.Match pat (TH.NormalB body) []
    let autoBody = TH.CaseE (VarE termVar) autoMatches
    -- define the case match on the result of the override
    termVar' <- TH.newName "term"
    let override = VarE (TH.mkName $ "on" ++ fromUpName nontermName)
                   `AppE` (VarE xlateVar)
                   `AppE` (VarE termVar)
        ovrMatches =
          [ TH.Match (TH.ConP 'Just [] [TH.VarP termVar']) (TH.NormalB $ VarE termVar') []
          , TH.Match (TH.ConP 'Nothing [] []) (TH.NormalB autoBody) []
          ]
    -- tie it all together
    let body = TH.CaseE override ovrMatches
        clause = TH.Clause [TH.VarP xlateVar, TH.VarP termVar] (TH.NormalB body) []
    let delegateId = TH.VarE funName `AppE` (TH.VarE (TH.mkName "idXlate") `AppE` TH.VarE xlateVar)
        bodyId = TH.InfixE (Just $ TH.VarE 'runIdentity) (TH.VarE '(.)) (Just delegateId)
        clauseId = TH.Clause [TH.VarP xlateVar] (TH.NormalB bodyId) []
    -- generate a type signature
    let quantifier = flip TH.PlainTV TH.InferredSpec <$> xdef.xlateParams ++ [xdef.xlateFParam]
        appClass = TH.ConT ''Applicative `AppT` TH.VarT xdef.xlateFParam
        xlateArgTyCon = TH.ConT $ TH.mkName "Xlate"
        xlateArgTy = foldl AppT xlateArgTyCon (TH.VarT <$> xdef.xlateParams ++ [xdef.xlateFParam])
        l1ArgTyCon = TH.ConT $ TH.mkName . fromUpDotName $ upDotChBase l1.langName.name nontermName
        l1ArgTy = foldl AppT l1ArgTyCon (TH.VarT . (.th) <$> l1.langInfo.langParams)
        l2ResTyCon = TH.ConT $ TH.mkName . fromUpDotName $ upDotChBase l2.langName.name nontermName
        l2ResTyCore = foldl AppT l2ResTyCon (TH.VarT . (.th) <$> l2.langInfo.langParams)
        l2ResTy = AppT (TH.VarT xdef.xlateFParam) l2ResTyCore
    let quantifierId = flip TH.PlainTV TH.InferredSpec <$> xdef.xlateParams
        xlateArgTyConId = TH.ConT $ TH.mkName "XlateI"
        xlateArgTyId = foldl AppT xlateArgTyConId (TH.VarT <$> xdef.xlateParams)
        l2ResTyId = l2ResTyCore
    -- and emit both signature and definition
    pure
      [ TH.SigD funName $ TH.ForallT quantifier [appClass] $
          xlateArgTy `ArrT` (l1ArgTy `ArrT` l2ResTy)
      , TH.FunD funName [clause]
      -- the "pure" (i.e. non-applicative) version
      , TH.SigD funNameId $ TH.ForallT quantifierId [] $
          xlateArgTyId `ArrT` (l1ArgTy `ArrT` l2ResTyId)
      , TH.FunD funNameId [clauseId]
      ]

---------------------
------ Helpers ------
---------------------

pattern ArrT :: TH.Type -> TH.Type -> TH.Type
pattern ArrT a b = AppT (AppT TH.ArrowT a) b

idiomAppE :: Exp -> Exp -> Exp
idiomAppE a b = TH.InfixE (Just a) (VarE '(<*>)) (Just b)

bang :: TH.Bang
bang = TH.Bang TH.NoSourceUnpackedness TH.SourceStrict

containsGrammar :: TypeDesc 'Valid -> Bool
containsGrammar (RecursiveType _) = True
containsGrammar (VarType _) = False
containsGrammar (CtorType _ ts) = any containsGrammar ts
containsGrammar (ListType t) = containsGrammar t
containsGrammar (MaybeType t) = containsGrammar t
containsGrammar (NonEmptyType t) = containsGrammar t
containsGrammar UnitType = False
containsGrammar (TupleType t1 t2 ts) = any containsGrammar (t1:t2:ts)

findAuto :: UpName -> UpName -> [XlateProd] -> Maybe XlateProd
findAuto sName pName autosHoles = case filter f autosHoles of
  [] -> Nothing
  x:_ -> Just x
  where
  f :: XlateProd -> Bool
  f (Left x) = x.nontermName == sName && x.prodName == pName
  f (Right x) = x.nontermName == sName && x.prodName == pName

hoistNothing :: Monad m => MaybeT m a
hoistNothing = MaybeT $ pure Nothing

base26 :: [String]
base26 = concat $ digits <$> ([0..] :: [Int])
  where
  digits n = (:) <$> ['a'..'z'] <*> (if n == 0 then pure "" else digits (n - 1))
