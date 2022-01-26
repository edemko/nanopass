{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Language.Haskell.TH.Syntax as TH


---------------------------------
------ Language Definition ------
---------------------------------

data LangDef = LangDef
  { langNameReq :: String
  , langParamReqs :: [String]
  , grammarReqs :: [GrammarDef]
  }
  deriving(Show)
data GrammarDef = GrammarDef
  { grammarNameReq :: String
  , ctorReqs :: [CtorDef]
  }
  deriving(Show)
data CtorDef = CtorDef
  { ctorNameReq :: String
  , subtermReqs :: [SubtermDef]
  }
  deriving(Show)
data SubtermDef = SubtermDef
  { subtermNameReq :: Maybe String
  , subtermType :: TypeDesc
  }
  deriving(Show)
data TypeDesc
  = GrammarType String -- these are metavariables that start with a lowercase letter
  | VarType TH.Name
  | CtorType TH.Name [TypeDesc] -- the string here will be used to look up a type in scope at the splice site, and will start with an uppercase letter
  | ListType TypeDesc -- because otherwise, you'd have to always be saying `type List a = [a]`
  | MaybeType TypeDesc
  | NonEmptyType TypeDesc
  | TupleType TypeDesc TypeDesc [TypeDesc]
  | MapType TypeDesc TypeDesc
  deriving(Eq,Show)

type Define a = StateT DefState Q a
data DefState = DefState
  { langTyvars :: [TH.Name]
  , grammarNames :: Map String TH.Name
  }

runDefine :: Define a -> Q a
runDefine = flip evalStateT st0
  where
  st0 = DefState
    { langTyvars = errorWithoutStackTrace "internal nanopass error: uninitialized langTyVars"
    , grammarNames = Map.empty
    }


defineLang :: LangDef -> Define [Dec]
defineLang LangDef{langNameReq,langParamReqs,grammarReqs} = do
  -- initialize language type variables
  let duplicateParams = langParamReqs \\ nub langParamReqs
  if not (null duplicateParams)
    then fail $ concat
      [ "in a nanopass language definition: "
      , "duplicate type parameter names "
      , show (nub duplicateParams)
      ]
    else modify $ \st -> st{ langTyvars = TH.mkName <$> langParamReqs }
  -- initialize grammarNames
  forM_ (grammarNameReq <$> grammarReqs) $ \grammarName -> do
    knownNames <- gets grammarNames
    case Map.lookup grammarName knownNames of
      Nothing -> modify $ \st ->
        st{grammarNames = Map.insert grammarName (TH.mkName grammarName) knownNames}
      Just _ -> fail $ concat [ "in a nanopass language definition: "
                              , "duplicate grammar (terminal/nonterminal) name "
                              , grammarName
                              ]
  -- define a type with one nullary ctor for every grammatical type
  langInfo <- defineLanginfo langNameReq
  -- define every nonterminal type
  params <- gets (fmap TH.PlainTV . langTyvars)
  types <- forM grammarReqs $ \(GrammarDef grammarName ctorReqs) -> do
    ctors <- defineCtor `mapM` ctorReqs
    pure $ TH.DataD [] (TH.mkName grammarName) params Nothing
            ctors
            []
  pure $ langInfo : types

defineLanginfo :: String -> Define Dec
defineLanginfo langName = do
  grammarNames <- gets $ Map.toAscList . grammarNames
  ctors <- forM grammarNames $ \(grammarName, _) -> do
    pure $ TH.NormalC (TH.mkName $ langName ++ "_" ++ grammarName) []
  let thName = TH.mkName langName
      -- I'm not sure I need these singe this type is just a glorified set of pointers, but here they are for reference
      -- dShow = TH.DerivClause Nothing [TH.ConT ''Show]
      -- dRead = TH.DerivClause Nothing [TH.ConT ''Read]
  pure $ TH.DataD [] thName [] Nothing ctors []

defineCtor :: CtorDef -> Define TH.Con
defineCtor (CtorDef termName memberRequests) = do
  let members = memberRequests <&> \case
        SubtermDef (Just explicitName) v -> (explicitName, v)
        SubtermDef Nothing v -> ("un" ++ termName, v)
  let duplicateNames = (fst <$> members) \\ nub (fst <$> members)
  fields <- case duplicateNames of
    [] -> mapM defineMember members
    _ -> fail $ concat [ "in a nanopass language definition: "
                       , "the following members were defined more than once in a term constructor"
                       , show (nub duplicateNames)
                       ]
  pure $ TH.RecC (TH.mkName termName) fields

defineMember :: (String, TypeDesc) -> Define TH.VarBangType
defineMember (lName, typeDesc) = do
  ty <- defineArg typeDesc
  pure (TH.mkName lName, noBang, ty)

defineArg :: TypeDesc -> Define TH.Type
defineArg (GrammarType lName) =
  gets (Map.lookup lName . grammarNames) >>= \case
    Just thName -> do
      let grammarCtor = TH.ConT thName
      params <- gets $ fmap TH.VarT . langTyvars
      pure $ foldl TH.AppT grammarCtor params
      -- pure $ TH.AppT grammarCtor params
    Nothing -> fail $ concat ["in a nanopass language definition: unknown metavariable ", lName]
defineArg (VarType vName) =
  gets ((vName `elem`) . langTyvars) >>= \case
    True -> do
      pure $ TH.VarT vName
    False -> fail $ concat ["in a nanopass language definition: unknown langauge parameter ", show vName]
defineArg (CtorType thName argDescs) = do
  args <- defineArg `mapM` argDescs
  pure $ foldl TH.AppT (TH.ConT thName) args
defineArg (ListType argDesc) = do
  arg <- defineArg argDesc
  pure $ TH.AppT TH.ListT arg
defineArg (NonEmptyType argDesc) = do
  neType <- M.lift [t|NonEmpty|]
  arg <- defineArg argDesc
  pure $ TH.AppT neType arg
defineArg (MaybeType argDesc) = do
  maybeType <- M.lift [t|Maybe|]
  arg <- defineArg argDesc
  pure $ TH.AppT maybeType arg
defineArg (TupleType t1 t2 ts) = do
  let tupLen = 2 + length ts
      thTup = TH.TupleT tupLen
  tys <- defineArg `mapM` (t1:t2:ts)
  pure $ foldl TH.AppT thTup tys
defineArg (MapType kDesc vDesc) = do
  m <- M.lift [t|Map|]
  k <- defineArg kDesc
  v <- defineArg vDesc
  pure $ TH.AppT (TH.AppT m k) v

----------------------------------
------ Language Reification ------
----------------------------------

data DefdLang = DefdLang
  { langQualPrefix :: String -- module name (including the dot before the basename) as requested in LangMod
  , thLangName :: TH.Name
  , thLangParams :: [TH.Name]
  , defdGrammars :: Map String DefdGrammarType
  }
  deriving(Show)
data DefdGrammarType = DefdGrammarType
  { thGrammarName :: TH.Name
  , defdCtors :: Map String DefdCtor
  }
  deriving(Show)
data DefdCtor = DefdCtor
  { thTermName :: TH.Name
  , defdArgs :: [DefdSubterm]
  }
  deriving(Show)
data DefdSubterm = DefdSubterm
  { thSubtermName :: TH.Name
  , defdSubtermType :: TypeDesc
  }
  deriving(Show)

-- given a string, we need to find the language info with that name in scope,
-- then decode each of the info's constructors into the names of grammar types,
-- then decode each grammar type
reifyLang :: String -> Q DefdLang
reifyLang langName = do
  (thLangName, grammarPtrs) <- findLangInfo
  -- determine the language's grammar types
  thGrammarTypes <- findGrammarType `mapM` grammarPtrs
  let gNames = thGrammarTypes <&> \(qualGName, _, _) -> qualGName
  grammarTypeList <- forM thGrammarTypes $ \(qualGrammarName, paramNames, thCtors) -> do
    ctorList <- decodeCtor gNames paramNames `mapM` thCtors
    let ctors = ctorList <&> \ctor -> ((TH.nameBase . thTermName) ctor, ctor)
        ctorNames = fst <$> ctors
        duplicateCNames = ctorNames \\ nub ctorNames
    case duplicateCNames of
      [] -> pure $ DefdGrammarType qualGrammarName (Map.fromList ctors)
      _ -> fail $ "corrupt language has duplicate term ctor names: " ++ show (nub duplicateCNames)
  -- disallowing duplicates here allows `decodeType.recurse` to produce `GrammarType`s easily
  let grammarTypes = grammarTypeList <&> \t -> ((TH.nameBase . thGrammarName) t, t)
      grammarNames = fst <$> grammarTypes
      duplicateGNames = grammarNames \\ nub grammarNames
  when (not $ null duplicateGNames) $ fail $
    "corrupt language has duplicate grammar names: " ++ show (nub duplicateGNames)
  -- determine the language's type parameters
  thLangParams <-
    let f Nothing (_, tvs, _) = pure (Just $ fixup <$> tvs)
        f (Just tvs) (_, tvs', _)
          | tvs == (fixup <$> tvs') = pure (Just tvs)
          | otherwise = fail $ concat
            [ "corrupt language has differing type paramaters between gramamr types. expected:\n"
            , "  " ++ show tvs ++ "\n"
            , "got:\n"
            , "  " ++ show (fixup <$> tvs')
            ]
     in fromMaybe [] <$> foldM f Nothing thGrammarTypes
  -- and we're done
  pure $ DefdLang
    { langQualPrefix
    , thLangName
    , thLangParams
    , defdGrammars = Map.fromList grammarTypes
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
  decodeCtor :: [TH.Name] -> [TH.Name] -> TH.Con -> Q DefdCtor
  decodeCtor gNames paramNames (TH.RecC thCtorName thFields) = do
    subterms <- forM thFields $ \(thFieldName, _, thSubtermType) -> do
      typeDesc <- decodeType gNames paramNames thSubtermType
      pure $ DefdSubterm thFieldName typeDesc
    pure $ DefdCtor thCtorName subterms
  decodeCtor _ _ otherCtor = fail $ "corrupt grammar ctor type:\n" ++ show otherCtor
  decodeType :: [TH.Name] -> [TH.Name] -> TH.Type -> Q TypeDesc
  decodeType gNames paramNames type0 = recurse type0
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
      , thName `elem` gNames && args == tvs
        -- we can just use TH.nameBase here, because in reifyLang, we make sure that there are no duplicates
        -- (there shouldn't be any duplicates anyway as long as language being decoded was generated by this library)
        = pure $ GrammarType (TH.nameBase thName)
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
    Just thLangName -> TH.reify thLangName >>= \case
      TH.TyConI (TH.DataD [] qualThLangName [] Nothing grammarNames _) -> pure (qualThLangName, grammarNames)
      otherInfo -> fail $ concat
        [ "in a nanopass language extension: base name " ++ langName ++ " does not identify a language: "
        , "  expecting language name to identify data definition, but got this type:\n"
        , "  " ++ show otherInfo
        ]
  findGrammarType :: TH.Con -> Q (TH.Name, [TH.Name], [TH.Con])
  findGrammarType (TH.NormalC thTypePtr []) = do
    let enumPrefix = langBase ++ "_"
    typePtrBase <- case stripPrefix enumPrefix (TH.nameBase thTypePtr) of
      Just it -> pure it
      Nothing -> fail $ concat
        [ "in a nanopass language extension: base name " ++ langBase ++ " does not identify a language: "
        , "  expecting language enum ctors to start with " ++ enumPrefix ++ ", but got name:\n"
        , "  " ++ TH.nameBase thTypePtr
        ]
    let typePtr = TH.mkName $ langQualPrefix ++ typePtrBase
    TH.reify typePtr >>= \case
      TH.TyConI (TH.DataD [] qualGrammarName thParams _ ctors _) -> do
        let thParamNames = thParams <&> \case { TH.PlainTV it -> it ; TH.KindedTV it _ -> it }
        pure (qualGrammarName, thParamNames, ctors)
      otherType -> fail $ "corrupt language grammar type:\n" ++ show otherType
  findGrammarType otherCtor = fail $ concat
    [ "in a nanopass language extension: base name " ++ langName ++ " does not identify a language: "
    , "  expecting language name to identify an enum, but got this constructor:\n"
    , "  " ++ show otherCtor
    ]

-- TODO I'll also need a way to modify a language so that it matches up to (and reuses the data types of) an existing language

--------------------------------
------ Language Extension ------
--------------------------------

data LangMod = LangMod
  { baseLang :: String
  , newLang :: String
  , newParams :: [String]
  , grammarMods :: [GrammarMod]
  }
  deriving(Show)
data GrammarMod
  = AddGrammar GrammarDef
  | DelGrammar String
  | ModCtors
    { grammarName :: String
    , ctorMods :: [CtorMod]
    }
  deriving(Show)
data CtorMod
  = AddCtor CtorDef
  | DelCtor String
  deriving(Show)

runModify :: LangMod -> Q [Dec]
runModify lMod = do
  oldLang <- reifyLang (baseLang lMod)
  modifyLang oldLang lMod

modifyLang :: DefdLang -> LangMod -> Q [Dec]
modifyLang defd mods = do
  defd' <- restrictLang defd (grammarMods mods)
  -- TODO I think it's at this point that I can generate the default translation
  lang' <- extendLang defd' mods
  runDefine $ defineLang lang'

restrictLang :: DefdLang -> [GrammarMod] -> Q DefdLang
restrictLang = foldM doGrammar
  where
  doGrammar :: DefdLang -> GrammarMod -> Q DefdLang
  doGrammar l (AddGrammar _) = pure l
  doGrammar l (DelGrammar gName) = case Map.lookup gName (defdGrammars l) of
    Just _ -> pure $ l{ defdGrammars = Map.delete gName (defdGrammars l) }
    Nothing -> fail $ concat
      [ "in nanopass language extention: "
      , "attempt to delete non-existent grammar type "
      , gName ++ " from " ++ show (thLangName l)
      ]
  doGrammar l (ModCtors gName ctorMods) = case Map.lookup gName (defdGrammars l) of
    Just grammar -> do
      grammar' <- foldM doCtors grammar ctorMods
      pure l{ defdGrammars = Map.insert gName grammar' (defdGrammars l) }
    Nothing -> fail $ concat
      [ "in nanopass language extension: "
      , "attempt to modify non-existent grammar type "
      , gName ++ " from " ++ show (thLangName l)
      ]
    where
    doCtors :: DefdGrammarType -> CtorMod -> Q DefdGrammarType
    doCtors g (AddCtor _) = pure g
    doCtors g (DelCtor cName) = case Map.lookup cName (defdCtors g) of
      Just _ -> pure $ g{ defdCtors = Map.delete cName (defdCtors g) }
      Nothing -> fail $ concat
        [ "in nanopass language extention: "
        , "attempt to delete non-existent term constructor "
        , gName ++ " from " ++ show (thGrammarName g) ++ " in " ++ show (thLangName l)
        ]

extendLang :: DefdLang -> LangMod -> Q LangDef
extendLang DefdLang{defdGrammars} lMods = do
  grammarReqs0 <- doGrammar (grammarMods lMods) `mapM` Map.elems defdGrammars
  let grammarReqs = grammarReqs0 ++ catAddGrammars (grammarMods lMods)
  pure $ LangDef
    { langNameReq = newLang lMods
    , langParamReqs = newParams lMods
    , grammarReqs
    }
  where
  doGrammar :: [GrammarMod] -> DefdGrammarType -> Q GrammarDef
  doGrammar gMods DefdGrammarType{thGrammarName,defdCtors} = do
    let ctorReqs0 = doCtor <$> Map.elems defdCtors
    let ctorReqs = ctorReqs0 ++ catAddCtors thGrammarName gMods
    pure GrammarDef{grammarNameReq = TH.nameBase thGrammarName, ctorReqs}
  doCtor :: DefdCtor -> CtorDef
  doCtor DefdCtor{thTermName, defdArgs} =
    CtorDef (TH.nameBase thTermName) (doSubterm <$> defdArgs)
  doSubterm :: DefdSubterm -> SubtermDef
  doSubterm DefdSubterm{thSubtermName, defdSubtermType} =
    SubtermDef (Just $ TH.nameBase thSubtermName) defdSubtermType
  catAddGrammars (AddGrammar g : moreGMods) = g : catAddGrammars moreGMods
  catAddGrammars (_ : moreGMods) = catAddGrammars moreGMods
  catAddGrammars [] = []
  catAddCtors thName (ModCtors toName ctorMods : moreGMods)
    | toName == TH.nameBase thName = go ctorMods ++ catAddCtors thName moreGMods
    where
    go (AddCtor c : moreCMods) = c : go moreCMods
    go (_ : moreCMods) = go moreCMods
    go [] = []
  catAddCtors thName (_ : moreCMods) = catAddCtors thName moreCMods
  catAddCtors _ [] = []


------------------------
------ TH Helpers ------
------------------------

noBang :: TH.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

