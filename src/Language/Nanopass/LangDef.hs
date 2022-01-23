{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Nanopass.LangDef where

import Control.Monad (forM,forM_,foldM)
import Control.Monad.State (StateT,gets,modify,evalStateT)
import Data.Functor ((<&>))
import Data.List (nub,(\\),stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Language.Haskell.TH (Q, Dec)

import qualified Control.Monad.Trans as M
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH


---------------------------------
------ Language Definition ------
---------------------------------

data LangDef = LangDef
  { langNameReq :: String
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
  | CtorType TH.Name [TypeDesc] -- the string here will be used to look up a type in scope at the splice site, and will start with an uppercase letter
  | ListType TypeDesc -- because otherwise, you'd have to always be saying `type List a = [a]`
  | MaybeType TypeDesc
  | NonEmptyType TypeDesc
  | TupleType TypeDesc TypeDesc [TypeDesc]
  | AlistType TypeDesc TypeDesc
  | MapType TypeDesc TypeDesc
  deriving(Show)

type Define a = StateT DefState Q a
data DefState = DefState
  { annotationType :: TH.Name
  , grammarNames :: Map String TH.Name
  }

runDefine :: Define a -> Q a
runDefine = flip evalStateT (DefState (error "use annotation type before init") Map.empty)


defineLang :: LangDef -> Define [Dec]
defineLang (LangDef langName grammars) = do
  -- initialize grammarNames
  forM_ (grammarNameReq <$> grammars) $ \grammarName -> do
    knownNames <- gets grammarNames
    case Map.lookup grammarName knownNames of
      Nothing -> modify $ \st ->
        st{grammarNames = Map.insert grammarName (TH.mkName grammarName) knownNames}
      Just _ -> fail $ concat [ "in a nanopass language definition: "
                              , "duplicate grammar (terminal/nonterminal) name "
                              , grammarName
                              ]
  -- define a type with one nullary ctor for every grammatical type
  langInfo <- defineLanginfo langName
  -- determine the type classes to derive
  let dFunctor = TH.DerivClause Nothing [TH.ConT ''Functor]
  derives <- M.lift $ TH.isExtEnabled TH.DeriveFunctor >>= \case
    True -> pure [dFunctor]
    False -> do
      TH.reportWarning $ unlines
        [ "There will be no Functor instance "
        , "    for the nanopass language " ++ langName
        , "    which is useful to update annotations independently of the syntax tree structure."
        , "    You likely want to enable the DeriveFunctor extension."
        ]
      pure []
  -- define every nonterminal type
  types <- forM grammars $ \(GrammarDef grammarName ctorReqs) -> do
    -- create a new type variable for annotations
    annTy <- M.lift (TH.newName "ann")
    modify $ \st -> st{annotationType = annTy}
    ctors <- defineCtor `mapM` ctorReqs
    pure $ TH.DataD [] (TH.mkName grammarName) [TH.PlainTV annTy] Nothing
            ctors
            derives
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
  members <- sequence $ memberRequests <&> \case
    SubtermDef (Just explicitName) v
      | explicitName == "annotation" -> fail $ concat [ "in a nanopass language definition: "
                                                      , "a term member cannot be named 'annotation'"
                                                      ]
      | otherwise -> pure (explicitName, v)
    SubtermDef Nothing v ->
      let gensym = "un" ++ termName
       in pure (gensym, v)
  let duplicateNames = (fst <$> members) \\ nub (fst <$> members)
  annField <- do
    annTy <- gets annotationType
    pure (TH.mkName "annotation", noBang, TH.VarT annTy)
  fields <- case duplicateNames of
    [] -> mapM defineMember members
    _ -> fail $ concat [ "in a nanopass language definition: "
                       , "the following members were defined more than once in a term constructor"
                       , show (nub duplicateNames)
                       ]
  pure $ TH.RecC (TH.mkName termName) (annField : fields)

defineMember :: (String, TypeDesc) -> Define TH.VarBangType
defineMember (lName, typeDesc) = do
  ty <- defineArg typeDesc
  pure (TH.mkName lName, noBang, ty)

defineArg :: TypeDesc -> Define TH.Type
defineArg (GrammarType lName) =
  gets (Map.lookup lName . grammarNames) >>= \case
    Just thName -> do
      let grammarCtor = TH.ConT thName
      ann <- TH.VarT <$> gets annotationType
      pure $ TH.AppT grammarCtor ann
    Nothing ->
      let lowName = case lName of { c:cs -> Char.toLower c : cs ; [] -> [] }
       in fail $ concat ["in a nanopass language definition: unknown metavariable ", lowName]
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
defineArg (AlistType kDesc vDesc) = do
  k <- defineArg kDesc
  v <- defineArg vDesc
  pure $ TH.AppT TH.ListT $ TH.AppT (TH.AppT (TH.TupleT 2) k) v
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
  (qualThLangName, grammarPtrs) <- findLangInfo
  thGrammarTypes <- findGrammarType `mapM` grammarPtrs
  grammarTypeList <- forM thGrammarTypes $ \(qualGrammarName, annName, thCtors) -> do
    ctorList <- decodeCtor annName `mapM` thCtors
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
  case duplicateGNames of
    [] -> pure $ DefdLang langQualifier qualThLangName (Map.fromList grammarTypes)
    _ -> fail $ "corrupt language has duplicate grammar names: " ++ show (nub duplicateGNames)
  where
  langQualifier = reverse . dropWhile (/= '.') . reverse $ langName
  langBase = reverse . takeWhile (/= '.') . reverse $ langName
  decodeCtor :: TH.Name -> TH.Con -> Q DefdCtor
  decodeCtor annName (TH.RecC thCtorName thFields) = do
    thUserFields <- case thFields of
      ((aName, _, TH.VarT aType):rest)
        | TH.nameBase aName == "annotation"
        && annName == aType
        -> pure rest
      _ -> fail "corrupt grammar ctor type: lacking an initial annotation field"
    subterms <- forM thUserFields $ \(thFieldName, _, thSubtermType) -> do
      typeDesc <- decodeType annName thSubtermType
      pure $ DefdSubterm thFieldName typeDesc
    pure $ DefdCtor thCtorName subterms
  decodeCtor _ otherCtor = fail $ "corrupt grammar ctor type:\n" ++ show otherCtor
  decodeType :: TH.Name -> TH.Type -> Q TypeDesc
  decodeType annName type0 = recurse type0
    where
    recurse (TH.ConT thName) = pure $ CtorType thName []
    recurse (TH.AppT (TH.ConT g) (TH.VarT ann)) | ann == annName = do
      -- we can just use TH.nameBase here, because in reifyLang, we make sure that there are no duplicates
      -- (there shouldn't be any duplicates anyway as long as language being decoded was generated by this library)
      pure $ GrammarType (TH.nameBase g)
    recurse (TH.AppT TH.ListT inner) = ListType <$> recurse inner
    recurse (TH.AppT inner thLastArg) = do
      lastArg <- recurse thLastArg
      recurse inner >>= \case
        CtorType f args -> pure $ CtorType f (args ++ [lastArg])
        otherType -> fail $ "corrupt type has kind error applying:\n" ++ show otherType ++ "\nto the argument:\n" ++ show lastArg
    recurse otherType = fail $ "corrupt subterm type:\n" ++ show otherType ++ "\n in type:\n" ++ show type0
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
  findGrammarType :: TH.Con -> Q (TH.Name, TH.Name, [TH.Con])
  findGrammarType (TH.NormalC thTypePtr []) = do
    let enumPrefix = langBase ++ "_"
    typePtrBase <- case stripPrefix enumPrefix (TH.nameBase thTypePtr) of
      Just it -> pure it
      Nothing -> fail $ concat
        [ "in a nanopass language extension: base name " ++ langBase ++ " does not identify a language: "
        , "  expecting language enum ctors to start with " ++ enumPrefix ++ ", but got name:\n"
        , "  " ++ TH.nameBase thTypePtr
        ]
    let typePtr = TH.mkName $ langQualifier ++ typePtrBase
    TH.reify typePtr >>= \case
      TH.TyConI (TH.DataD [] qualGrammarName [annTy] _ ctors _) -> do
        let annName = case annTy of { TH.PlainTV it -> it ; TH.KindedTV it _ -> it }
        pure (qualGrammarName, annName, ctors)
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
  pure $ LangDef{langNameReq = newLang lMods, grammarReqs}
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

