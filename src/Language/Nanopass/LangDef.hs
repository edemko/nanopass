{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Nanopass.LangDef where

import Control.Monad (forM,forM_,foldM,when)
import Control.Monad.State (StateT,gets,modify,evalStateT)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List (nub,(\\),stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (Q, Dec)

import qualified Control.Monad.Trans as M
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH

data TypeDesc
  = RecursiveType String -- these are metavariables that start with a lowercase letter
  | VarType TH.Name
  | CtorType TH.Name [TypeDesc] -- the string here will be used to look up a type in scope at the splice site, and will start with an uppercase letter
  | ListType TypeDesc -- because otherwise, you'd have to always be saying `type List a = [a]`
  | MaybeType TypeDesc
  | NonEmptyType TypeDesc
  | TupleType TypeDesc TypeDesc [TypeDesc]
  | MapType TypeDesc TypeDesc
  deriving(Eq,Show)

---------------------------------
------ Language Definition ------
---------------------------------

data LangDef = LangDef
  { langNameReq :: String
  , langParamReqs :: [String]
  , syncatReqs :: [SyncatDef]
  }
  deriving(Show)
data SyncatDef = SyncatDef
  { syncatNameReq :: String
  , productionReqs :: [ProdDef]
  }
  deriving(Show)
data ProdDef = ProdDef
  { prodNameReq :: String
  , subtermReqs :: [SubtermDef]
  }
  deriving(Show)
data SubtermDef = SubtermDef
  { subtermNameReq :: Maybe String
  , subtermTypeReq :: TypeDesc
  }
  deriving(Show)

type Define a = StateT DefState Q a
data DefState = DefState
  { langTyvars :: [TH.Name]
  , syncatNames :: Map String TH.Name
  }

runDefine :: Define a -> Q a
runDefine = flip evalStateT st0
  where
  st0 = DefState
    { langTyvars = errorWithoutStackTrace "internal nanopass error: uninitialized langTyVars"
    , syncatNames = Map.empty
    }

defineLang :: LangDef -> Define [Dec]
defineLang l = do
  -- initialize language type variables
  let duplicateParams = l.langParamReqs \\ nub l.langParamReqs
  if not (null duplicateParams)
    then fail $ concat
      [ "in a nanopass language definition: "
      , "duplicate language parameter names "
      , show (nub duplicateParams)
      ]
    else modify $ \st -> st{ langTyvars = TH.mkName <$> l.langParamReqs }
  -- initialize syncatNames
  forM_ (syncatNameReq <$> l.syncatReqs) $ \syncatReq -> do
    knownNames <- gets syncatNames
    case Map.lookup syncatReq knownNames of
      Nothing -> modify $ \st ->
        st{syncatNames = Map.insert syncatReq (TH.mkName syncatReq) knownNames}
      Just _ -> fail $ concat [ "in a nanopass language definition: "
                              , "duplicate syntactic category (terminal/nonterminal) name "
                              , syncatReq
                              ]
  -- define a type with one nullary ctor for every grammatical type
  langInfo <- defineLanginfo l.langNameReq
  -- define every nonterminal type
  params <- gets langTyvars <&> \tvs -> tvs <&> \tv -> TH.PlainTV tv ()
  syncatTypeDecs <- forM l.syncatReqs $ \syn -> do
    prodCtors <- defineProduction `mapM` syn.productionReqs
    pure $ TH.DataD [] (TH.mkName syn.syncatNameReq) params Nothing
            prodCtors
            []
  pure $ langInfo : syncatTypeDecs

defineLanginfo :: String -> Define Dec
defineLanginfo langName = do
  syncatNames <- gets $ Map.toAscList . syncatNames
  ctors <- forM syncatNames $ \(syncatName, _) -> do
    pure $ TH.NormalC (TH.mkName $ langName ++ "_" ++ syncatName) []
  let thName = TH.mkName langName
      -- I'm not sure I need these singe this type is just a glorified set of pointers, but here they are for reference
      -- dShow = TH.DerivClause Nothing [TH.ConT ''Show]
      -- dRead = TH.DerivClause Nothing [TH.ConT ''Read]
  pure $ TH.DataD [] thName [] Nothing ctors []

defineProduction :: ProdDef -> Define TH.Con
defineProduction production = do
  let members = production.subtermReqs <&> \case
        SubtermDef (Just explicitName) v -> (explicitName, v)
        SubtermDef Nothing v -> ("un" ++ production.prodNameReq, v)
  let duplicateNames = (fst <$> members) \\ nub (fst <$> members)
  fields <- case duplicateNames of
    [] -> mapM defineSubterm members
    _ -> fail $ concat [ "in a nanopass language definition: "
                       , "the following subterms were defined more than once in a production"
                       , show (nub duplicateNames)
                       ]
  pure $ TH.RecC (TH.mkName production.prodNameReq) fields

defineSubterm :: (String, TypeDesc) -> Define TH.VarBangType
defineSubterm (langName, typeDesc) = do
  ty <- subtermType typeDesc
  pure (TH.mkName langName, noBang, ty)

subtermType :: TypeDesc -> Define TH.Type
subtermType (RecursiveType lName) =
  gets (Map.lookup lName . syncatNames) >>= \case
    Just thName -> do
      let grammarCtor = TH.ConT thName
      params <- gets $ fmap TH.VarT . langTyvars
      pure $ foldl TH.AppT grammarCtor params
      -- pure $ TH.AppT grammarCtor params
    Nothing -> fail $ concat ["in a nanopass language definition: unknown metavariable ", lName]
subtermType (VarType vName) =
  gets ((vName `elem`) . langTyvars) >>= \case
    True -> do
      pure $ TH.VarT vName
    False -> fail $ concat ["in a nanopass language definition: unknown langauge parameter ", show vName]
subtermType (CtorType thName argDescs) = do
  args <- subtermType `mapM` argDescs
  pure $ foldl TH.AppT (TH.ConT thName) args
subtermType (ListType argDesc) = do
  arg <- subtermType argDesc
  pure $ TH.AppT TH.ListT arg
subtermType (NonEmptyType argDesc) = do
  neType <- M.lift [t|NonEmpty|]
  arg <- subtermType argDesc
  pure $ TH.AppT neType arg
subtermType (MaybeType argDesc) = do
  maybeType <- M.lift [t|Maybe|]
  arg <- subtermType argDesc
  pure $ TH.AppT maybeType arg
subtermType (TupleType t1 t2 ts) = do
  let tupLen = 2 + length ts
      thTup = TH.TupleT tupLen
  tys <- subtermType `mapM` (t1:t2:ts)
  pure $ foldl TH.AppT thTup tys
subtermType (MapType kDesc vDesc) = do
  m <- M.lift [t|Map|]
  k <- subtermType kDesc
  v <- subtermType vDesc
  pure $ TH.AppT (TH.AppT m k) v

----------------------------------
------ Language Reification ------
----------------------------------

data DefdLang = DefdLang
  { langQualPrefix :: String -- module name (including the dot before the basename) as requested in LangMod
  , defdLangName :: TH.Name
  , defdLangParams :: [TH.Name]
  , defdSyncats :: Map String DefdSyncatType
  }
  deriving(Show)
data DefdSyncatType = DefdSyncatType
  { defdSyncatName :: TH.Name
  , defdProds :: Map String DefdProd
  }
  deriving(Show)
data DefdProd = DefdProd
  { defdProdName :: TH.Name
  , defdSubterms :: [DefdSubterm]
  }
  deriving(Show)
data DefdSubterm = DefdSubterm
  { defdSubtermName :: TH.Name
  , defdSubtermType :: TypeDesc
  }
  deriving(Show)

-- given a string, we need to find the language info with that name in scope,
-- then decode each of the info's constructors into the names of grammar types,
-- then decode each grammar type
reifyLang :: String -> Q DefdLang
reifyLang langName = do
  (defdLangName, syncatPtrs) <- findLangInfo
  -- determine the language's grammar types
  thSyncats <- findRecursiveType `mapM` syncatPtrs
  let sNames = thSyncats <&> \(qualSName, _, _) -> qualSName
  syncatTypeList <- forM thSyncats $ \(qualSyncatName, paramNames, thCtors) -> do
    ctorList <- decodeCtor sNames paramNames `mapM` thCtors
    let productions = ctorList <&> \ctor -> ((TH.nameBase . defdProdName) ctor, ctor)
        prodNames = fst <$> productions
        duplicatePNames = prodNames \\ nub prodNames
    case duplicatePNames of
      [] -> pure DefdSyncatType
        { defdSyncatName = qualSyncatName
        , defdProds = Map.fromList productions
        }
      _ -> fail $ "corrupt language has duplicate production names: " ++ show (nub duplicatePNames)
  -- disallowing duplicates here allows `decodeType.recurse` to produce `RecursiveType`s easily
  let syncatTypes = syncatTypeList <&> \t -> ((TH.nameBase . defdSyncatName) t, t)
      syncatNames = fst <$> syncatTypes
      duplicateSNames = syncatNames \\ nub syncatNames
  when (not $ null duplicateSNames) $ fail $
    "corrupt language has duplicate syntactic category names: " ++ show (nub duplicateSNames)
  -- determine the language's type parameters
  defdLangParams <-
    let f Nothing (_, tvs, _) = pure (Just $ fixup <$> tvs)
        f (Just tvs) (_, tvs', _)
          | tvs == (fixup <$> tvs') = pure (Just tvs)
          | otherwise = fail $ concat
            [ "corrupt language has differing paramaters between syntactic categories. expected:\n"
            , "  " ++ show tvs ++ "\n"
            , "got:\n"
            , "  " ++ show (fixup <$> tvs')
            ]
     in fromMaybe [] <$> foldM f Nothing thSyncats
  -- and we're done
  pure $ DefdLang
    { langQualPrefix
    , defdLangName
    , defdLangParams
    , defdSyncats = Map.fromList syncatTypes
    }
  where
  -- this is here because TH will add a bunch of garbage on the end of a type variable to ensure it doesn't capture,
  -- but in this case I _want_ it to capture, so I can check name equality across different types
  fixup :: TH.Name -> TH.Name
  fixup = TH.mkName . reverse . loop . reverse . show
    where
    loop (c:rest)
      | c == '_' = rest
      | '0' <= c && c <= '9' = loop rest
    loop other = other
  langQualPrefix = reverse . dropWhile (/= '.') . reverse $ langName
  langBase = reverse . takeWhile (/= '.') . reverse $ langName
  decodeCtor :: [TH.Name] -> [TH.Name] -> TH.Con -> Q DefdProd
  decodeCtor sNames paramNames (TH.RecC defdProdName thFields) = do
    defdSubterms <- forM thFields $ \(thFieldName, _, thSubtermType) -> do
      typeDesc <- decodeType sNames paramNames thSubtermType
      pure $ DefdSubterm thFieldName typeDesc
    pure $ DefdProd{defdProdName,defdSubterms}
  decodeCtor _ _ otherCtor = fail $ "corrupt production type:\n" ++ show otherCtor
  decodeType :: [TH.Name] -> [TH.Name] -> TH.Type -> Q TypeDesc
  decodeType sNames paramNames type0 = recurse type0
    where
    tvs = TH.VarT <$> paramNames
    recurse tuple | Just (t1:t2:ts) <- fromTuple tuple = do
      t1Desc <- recurse t1
      t2Desc <- recurse t2
      tDescs <- recurse `mapM` ts
      pure $ TupleType t1Desc t2Desc tDescs
    recurse (TH.AppT (TH.AppT (TH.ConT special) k) v)
      | special == ''Map = MapType <$> recurse k <*> recurse v
    recurse (TH.AppT (TH.ConT special) a)
      | special == ''Maybe = MaybeType <$> recurse a
      | special == ''NonEmpty = NonEmptyType <$> recurse a
    recurse (TH.AppT TH.ListT a) = ListType <$> recurse a
    recurse appType
      | (TH.ConT thName, args) <- fromApps appType
      , thName `elem` sNames && args == tvs
        -- we can just use TH.nameBase here, because in reifyLang, we make sure that there are no duplicates
        -- (there shouldn't be any duplicates anyway as long as language being decoded was generated by this library)
        = pure $ RecursiveType (TH.nameBase thName)
      | (TH.ConT thName, args) <- fromApps appType = do
        decodedArgs <- recurse `mapM` args
        pure $ CtorType thName decodedArgs
    recurse (TH.VarT a) = pure $ VarType a
    recurse otherType = fail $ "corrupt subterm type:\n" ++ show otherType ++ "\n in type:\n" ++ show type0
    fromTuple :: TH.Type -> Maybe [TH.Type]
    fromTuple t0 = case loop t0 of
      Just (0, ts) -> Just (reverse ts)
      _ -> Nothing
      where
      loop (TH.TupleT n) = Just (n, [])
      loop (TH.AppT f t)
        | Just (n, ts) <- loop f = Just (n - 1, t:ts)
      loop _ = Nothing
    fromApps :: TH.Type -> (TH.Type, [TH.Type])
    fromApps = second reverse . loop
      where
      loop (TH.AppT inner lastArg) = second (lastArg:) (loop inner)
      loop t = (t, [])
  findLangInfo :: Q (TH.Name, [TH.Con]) -- name and constructors of the info type
  findLangInfo = TH.lookupTypeName langName >>= \case
    Nothing -> fail $ "in a nanopass language extension: could not find base language " ++ langName
    Just defdLangName -> TH.reify defdLangName >>= \case
      TH.TyConI (TH.DataD [] qualThLangName [] Nothing syncatNames _) -> pure (qualThLangName, syncatNames)
      otherInfo -> fail $ concat
        [ "in a nanopass language extension: base name " ++ langName ++ " does not identify a language: "
        , "  expecting language name to identify data definition, but got this type:\n"
        , "  " ++ show otherInfo
        ]
  findRecursiveType :: TH.Con -> Q (TH.Name, [TH.Name], [TH.Con])
  findRecursiveType (TH.NormalC thTypePtr []) = do
    let enumPrefix = langBase ++ "_"
    typePtrBase <- case stripPrefix enumPrefix (TH.nameBase thTypePtr) of
      Just it -> pure it
      Nothing -> fail $ concat
        [ "in a nanopass language extension: base name " ++ langBase ++ " does not identify a language: "
        , "  expecting language info enum ctors to start with " ++ enumPrefix ++ ", but got name:\n"
        , "  " ++ TH.nameBase thTypePtr
        ]
    let typePtr = TH.mkName $ langQualPrefix ++ typePtrBase
    TH.reify typePtr >>= \case
      TH.TyConI (TH.DataD [] qualSyncatName thParams _ ctors _) -> do
        let thParamNames = thParams <&> \case { TH.PlainTV it _ -> it ; TH.KindedTV it _ _ -> it }
        pure (qualSyncatName, thParamNames, ctors)
      otherType -> fail $ "corrupt language syntactic category type:\n" ++ show otherType
  findRecursiveType otherCtor = fail $ concat
    [ "in a nanopass language extension: base name " ++ langName ++ " does not identify a language: "
    , "  expecting language name to identify an enum, but got this constructor:\n"
    , "  " ++ show otherCtor
    ]

--------------------------------
------ Language Extension ------
--------------------------------

data LangMod = LangMod
  { baseLangReq :: String
  , newLangReq :: String
  , newParamReqs :: [String]
  , syncatMods :: [SyncatMod]
  }
  deriving(Show)
data SyncatMod
  = AddSyncat SyncatDef
  | DelSyncat String
  | ModProds
    { syncatName :: String
    , prodMods :: [ProdMod]
    }
  deriving(Show)
data ProdMod
  = AddProd ProdDef
  | DelProd String
  deriving(Show)

runModify :: LangMod -> Q [Dec]
runModify lMod = do
  oldLang <- reifyLang (baseLangReq lMod)
  modifyLang oldLang lMod

modifyLang :: DefdLang -> LangMod -> Q [Dec]
modifyLang defd mods = do
  defd' <- restrictLang defd (syncatMods mods)
  -- TODO I think it's at this point that I can generate the default translation
  lang' <- extendLang defd' mods
  runDefine $ defineLang lang'

restrictLang :: DefdLang -> [SyncatMod] -> Q DefdLang
restrictLang = foldM doSyncat
  where
  doSyncat :: DefdLang -> SyncatMod -> Q DefdLang
  doSyncat l (AddSyncat _) = pure l
  doSyncat l (DelSyncat sName) = case Map.lookup sName l.defdSyncats of
    Just _ -> pure $ l{ defdSyncats = Map.delete sName l.defdSyncats }
    Nothing -> fail $ concat
      [ "in nanopass language extention: "
      , "attempt to delete non-existent syntactic category "
      , sName ++ " from " ++ show (defdLangName l)
      ]
  doSyncat l (ModProds sName prodMods) = case Map.lookup sName l.defdSyncats of
    Just syncat -> do
      syncat' <- foldM doProds syncat prodMods
      pure l{ defdSyncats = Map.insert sName syncat' l.defdSyncats }
    Nothing -> fail $ concat
      [ "in nanopass language extension: "
      , "attempt to modify non-existent syntactic category "
      , sName ++ " from " ++ show (defdLangName l)
      ]
    where
    doProds :: DefdSyncatType -> ProdMod -> Q DefdSyncatType
    doProds s (AddProd _) = pure s
    doProds s (DelProd pName) = case Map.lookup pName s.defdProds of
      Just _ -> pure $ s{ defdProds = Map.delete pName s.defdProds }
      Nothing -> fail $ concat
        [ "in nanopass language extention: "
        , "attempt to delete non-existent term constructor "
        , sName ++ " from " ++ show s.defdSyncatName ++ " in " ++ show l.defdLangName
        ]

extendLang :: DefdLang -> LangMod -> Q LangDef
extendLang l lMods = do
  syncatReqs0 <- doSyncat lMods.syncatMods `mapM` Map.elems l.defdSyncats
  let syncatReqs = syncatReqs0 ++ catAddSyncat lMods.syncatMods
  pure $ LangDef
    { langNameReq = lMods.newLangReq
    , langParamReqs = lMods.newParamReqs
    , syncatReqs
    }
  where
  doSyncat :: [SyncatMod] -> DefdSyncatType -> Q SyncatDef
  doSyncat gMods DefdSyncatType{defdSyncatName,defdProds} = do
    let productionReqs0 = doProd <$> Map.elems defdProds
    let productionReqs = productionReqs0 ++ catAddProd defdSyncatName gMods
    pure SyncatDef{syncatNameReq = TH.nameBase defdSyncatName, productionReqs}
  doProd :: DefdProd -> ProdDef
  doProd DefdProd{defdProdName, defdSubterms} =
    ProdDef (TH.nameBase defdProdName) (doSubterm <$> defdSubterms)
  doSubterm :: DefdSubterm -> SubtermDef
  doSubterm DefdSubterm{defdSubtermName, defdSubtermType} =
    SubtermDef (Just $ TH.nameBase defdSubtermName) defdSubtermType
  catAddSyncat (AddSyncat s : moreSMods) = s : catAddSyncat moreSMods
  catAddSyncat (_ : moreSMods) = catAddSyncat moreSMods
  catAddSyncat [] = []
  catAddProd thName (ModProds toName prodMods : moreSMods)
    | toName == TH.nameBase thName = go prodMods ++ catAddProd thName moreSMods
    where
    go (AddProd p : morePMods) = p : go morePMods
    go (_ : morePMods) = go morePMods
    go [] = []
  catAddProd thName (_ : morePMods) = catAddProd thName morePMods
  catAddProd _ [] = []


------------------------
------ TH Helpers ------
------------------------

noBang :: TH.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

