{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Nanopass.Internal.Parser
  (
  -- * Recognizers
    parseLanguage
  -- ** Base Languages
  , parseBaseLanguage
  , parseSyncat
  , parseProduction
  -- ** Language Modification
  , parseLangMod
  , parseSyncatsEdit
  , parseProductionsEdit
  -- ** Shared Recognizers
  , parseLangLHS
  , parseSyncatBody
  , parseProductionBody
  , parseTypeDesc
  -- * S-Expressions
  , getSexpr
  , Loc(..)
  , toUpColonName
  ) where

import Nanopass.Internal.Representation

import Control.Monad (forM)
import Data.Functor ((<&>))
import Text.Megaparsec (runParser',State(..),PosState(..),SourcePos(..),errorBundlePretty)
import Text.Megaparsec.Pos (defaultTabWidth,mkPos)
import Text.SExpression (SExpr(..),parseSExpr,def)

import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH

------------------------------
------ Glue it together ------
------------------------------

-- | @
-- Language ::= <BaseLang> | <LangMod>
-- @
parseLanguage :: (Loc, String) -> Either Error (Either Language LangMod)
parseLanguage inp@(_, orig) = do
  sexpr <- getSexpr inp
  case sexpr of
    List (_:Atom "from":_) -> Right <$> parseLangMod orig sexpr
    _ -> Left <$> parseBaseLanguage orig sexpr

---------------------------------
------ Recognize Languages ------
---------------------------------

-- | @
-- BaseLang ::=
--   (<LangLHS>
--     <string…>
--     <Syncat…>)
-- @
parseBaseLanguage :: String -> SExpr -> Either Error Language
parseBaseLanguage originalProgram (List (lhs:rest)) = do
  (langName, langParams) <- parseLangLHS lhs
  let (docs, syncats_) = spanDocstrs rest
  syncatList <- parseSyncat `mapM` syncats_
  let syncats = Map.fromList $ syncatList <&> \s -> (s.syncatName, s)
  pure Language
    { langName = unDotted langName
    , langNameTH = TH.mkName $ fromUpName langName
    , langParams
    , syncats
    , originalProgram = Just originalProgram
    , baseDefdLang = Nothing
    }
parseBaseLanguage _ other = Left $ ExpectingLanguage other

-- | @
-- LangLHS ::= <UpCase name>
--          |  (<UpCase name> <LowCase type var…>)
-- @
parseLangLHS :: SExpr -> Either Error (UpName, [LowName])
parseLangLHS (Atom str) = case toUpName str of
  Just name -> pure (name, [])
  Nothing -> Left $ ExpectedLangName str
parseLangLHS (List (Atom str:rest)) = do
  name <- case toUpName str of 
    Just name -> pure name
    Nothing -> Left $ ExpectedLangName str
  tyVars <- forM rest $ \case
    Atom tvStr | Just tvName <- toLowName tvStr -> pure tvName
    it -> Left $ ExpectingTypeVariable it
  pure (name, tyVars)
parseLangLHS it = Left $ ExpectedLangLHS it

-- | @
-- Syncat ::=
--   (<UpCase name>      type name
--     <string…>         documentation
--     <Production…>)    constructor arguments
-- @
parseSyncat :: SExpr -> Either Error Syncat
parseSyncat (List (Atom str:rest)) = do
  syncatName <- case toUpName str of
    Just name -> pure name
    Nothing -> Left $ ExpectedSyncatName (Just $ Atom str)
  parseSyncatBody syncatName rest
parseSyncat (List (other:_)) = Left $ ExpectedSyncatName (Just other)
parseSyncat other = Left $ ExpectedSyncat other

-- | Separated out from 'parseSyncat' because it is useful in 'parseSyncatsEdit' as well.
parseSyncatBody :: UpName -> [SExpr] -> Either Error Syncat
parseSyncatBody syncatName rest = do
  let (docs, prods_) = spanDocstrs rest
  productionList <- parseProduction `mapM` prods_
  let productions = Map.fromList $ productionList <&> \p -> (p.prodName, p)
  pure Syncat
    { syncatName
    , syncatNameTH = TH.mkName $ fromUpName syncatName
    , productions
    }

-- | @
-- Production ::=
--   (<UpCase name>      constructor name
--     <string…>         documentation
--     <Type…>)          constructor arguments
-- @
parseProduction :: SExpr -> Either Error Production
parseProduction (List (Atom ctorStr:rest)) = do
  prodName <- case toUpName ctorStr of
    Just name -> pure name
    Nothing -> Left $ ExpectedConstructorName (Just $ Atom ctorStr)
  parseProductionBody prodName rest
parseProduction other = Left $ ExpectedProduction other

-- | Separated out from 'parseProduction' because it is useful in 'parseProductionsEdit' as well.
parseProductionBody :: UpName -> [SExpr] -> Either Error Production
parseProductionBody prodName rest = do
  let (docs, args) = spanDocstrs rest
  subterms <- parseTypeDesc `mapM` args
  pure Production
    { prodName
    , prodNameTH = TH.mkName $ fromUpName prodName
    , subterms
    }

-- | @
-- Type ::= <UpCase name>                non-terminal
--       |  <lowCase name>               type parameter
--       |  (<UpDotName> <Type…>)        Haskell type
--       |  (? <type>)                   Maybe type
--       |  (* <type>)                   List type
--       |  (+ <type>)                   NonEmpty type
--       |  () | (&)                     unit type
--       |  (& <Type>)                   Only types TODO
--       |  (& <Type> <Type> <Type…>)    tuple types
-- @
parseTypeDesc :: SExpr -> Either Error TypeDesc
parseTypeDesc = \case
  Atom str
    | Just name <- toUpName str -> pure $ RecursiveType name
    | Just name <- toLowName str -> pure $ VarType (TH.mkName $ fromLowName name)
    | otherwise -> Left $ ExpectingTypeNameOrVar str
  List [] -> pure UnitType
  List [Atom "?", x] -> MaybeType <$> parseTypeDesc x
  List [Atom "*", x] -> ListType <$> parseTypeDesc x
  List [Atom "+", x] -> NonEmptyType <$> parseTypeDesc x
  List (Atom "&" : xs_) -> case xs_ of
    [] -> pure UnitType
    [x] -> parseTypeDesc x -- TODO Data.Tuple.Only
    (x1:x2:xs) -> do
      t1 <- parseTypeDesc x1
      t2 <- parseTypeDesc x2
      ts <- parseTypeDesc `mapM` xs
      pure $ TupleType t1 t2 ts
  List (x:xs) -> do
    ctor <- case x of
      Atom str | Just name <- toUpColonName str -> pure name
      _ -> Left $ ExpectedTypeConstructor x
    ts <- parseTypeDesc `mapM` xs
    pure $ CtorType (TH.mkName $ fromUpDotName ctor) ts
  x@(ConsList _ _) -> Left $ ConsListsDisallowed x
  x@(Number _) -> Left $ UnexpectedLiteral x
  x@(String _) -> Left $ UnexpectedLiteral x
  x@(Bool _) -> Left $ UnexpectedLiteral x

spanDocstrs :: [SExpr] -> ([String], [SExpr])
spanDocstrs = loop []
  where
  loop acc (String str:rest) = loop (str:acc) rest
  loop acc rest = (reverse acc, rest)

-----------------------------------
------ Language Modification ------
-----------------------------------

-- | @
-- LangMod ::=
--   (<LangLHS> 'from' <UpDot base language name>
--     <string…>
--     <LangEdit…>)
-- @
parseLangMod :: String -> SExpr -> Either Error LangMod
parseLangMod originalModProgram (List (lhs:Atom "from":rest_)) = do
  (newLang, newParams) <- parseLangLHS lhs
  (baseLang, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpColonName str -> pure (name, rest)
    other:_ -> Left $ ExpectingBaseLanguage (Just other)
    _ -> Left $ ExpectingBaseLanguage Nothing
  let (docs, edits_) = spanDocstrs rest
  edits <- parseSyncatsEdit `mapM` edits_
  pure LangMod
    { baseLang
    , newLang
    , newParams
    , syncatsEdit = edits
    , originalModProgram = Just originalModProgram
    }
parseLangMod _ other = Left $ ExpectingKwFromAfterLHS other

-- | @
-- SyncatsEdit
--   ::= ('+' <UpCase name> <string…> <Production…>)    add a syntactic category
--    |  ('-' <UpCase name>)                            remove a syntactic category
--    |  ('*' <UpCase name> <ProductionsEdit…>)         modify a syntactic category's productions
-- @
parseSyncatsEdit :: SExpr -> Either Error SyncatsEdit
parseSyncatsEdit (List (Atom "+":rest_)) = do
  (syncatName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedSyncatName (Just other)
    [] -> Left $ ExpectedSyncatName Nothing
  syncat <- parseSyncatBody syncatName rest
  pure $ AddSyncat syncat
parseSyncatsEdit (List (Atom "-":rest_)) = do
  (syncatName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedSyncatName (Just other)
    [] -> Left $ ExpectedSyncatName Nothing
  case rest of
    [] -> pure ()
    x:_ -> Left $ UnexpectedSExprAfterDelete x
  pure $ DelSyncat syncatName
parseSyncatsEdit (List (Atom "*":rest_)) = do
  (syncatName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedSyncatName (Just other)
    [] -> Left $ ExpectedSyncatName Nothing
  edits <- parseProductionsEdit `mapM` rest
  pure $ ModSyncat syncatName edits
parseSyncatsEdit (List (other:_)) = Left $ ExpectingPlusMinusStar other
parseSyncatsEdit other = Left $ ExpectingSyncatsEdit other

-- | @
-- ProductionsEdit
--   ::= ('+' <UpCase name> <string…> <Type…>)    add a production
--    |  ('-' <UpCase name>)                      remove a production
-- @
parseProductionsEdit :: SExpr -> Either Error ProductionsEdit
parseProductionsEdit (List (Atom "+":rest_)) = do
  (prodName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedConstructorName (Just other)
    [] -> Left $ ExpectedConstructorName Nothing
  prod <- parseProductionBody prodName rest
  pure $ AddProd prod
parseProductionsEdit (List (Atom "-":rest_)) = do
  (prodName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedConstructorName (Just other)
    [] -> Left $ ExpectedConstructorName Nothing
  case rest of
    [] -> pure ()
    x:_ -> Left $ UnexpectedSExprAfterDelete x
  pure $ DelProd prodName
parseProductionsEdit (List (other:_)) = Left $ ExpectingPlusMinus other
parseProductionsEdit other = Left $ ExpectingProductionsEdit other

---------------------------------
------ Parse S-Expressions ------
---------------------------------

-- | This is a location type that should be sufficient to describe the start of a Template Haskell quasiquote.
-- It is used in 'getSexpr' so that it can report errors from the actual source code location.
data Loc = Loc
  { file :: FilePath
  , line :: Int -- ^ TODO hopefully 1-indexed
  , col :: Int -- ^ TODO hopefully 1-indexed
  }

-- | This serves as an adapter between Template Haskell and whatever s-expression parser I decide to use.
getSexpr :: (Loc, String) -> Either Error SExpr
getSexpr (loc, inp) = case runParser' (parseSExpr def) state0 of
    (_, Left err) -> Left . SexprError $ errorBundlePretty err
    (_, Right sexpr) -> Right sexpr
  where
  state0 = State
    { stateInput = inp
    , stateOffset = 0
    , statePosState = PosState
      { pstateInput = inp
      , pstateOffset = 0
      , pstateSourcePos = SourcePos
        { sourceName = loc.file
        , sourceLine = mkPos loc.line
        , sourceColumn = mkPos loc.col
        }
      , pstateTabWidth = defaultTabWidth
      , pstateLinePrefix = ""
      }
    , stateParseErrors = []
    }

-- | Since sexprs don't allow dot in names, we use colon instead.
-- We just immediately translate it over into dots.
toUpColonName :: String -> Maybe UpDotName
toUpColonName = toUpDotName . map (\c -> if c == ':' then '.' else c)


-----------------------------
------ Error Reporting ------
-----------------------------

data Error
  = SexprError String
  -- parseLanguage
  | ExpectingLanguage SExpr
  | ExpectedLangLHS SExpr
  | ExpectedLangName String
  | ExpectingTypeVariable SExpr
  -- parseLanguageMod
  | ExpectingBaseLanguage (Maybe SExpr)
  | ExpectingKwFromAfterLHS SExpr
  | UnexpectedSExprAfterDelete SExpr
  | ExpectingPlusMinusStar SExpr
  | ExpectingSyncatsEdit SExpr
  | ExpectingPlusMinus SExpr
  | ExpectingProductionsEdit SExpr
  -- parseSyncat
  | ExpectedSyncatName (Maybe SExpr)
  | ExpectedSyncat SExpr
  -- parseProduction
  | ExpectedConstructorName (Maybe SExpr)
  | ExpectedProduction SExpr
  -- parseTypeDesc
  | UnrecognizedSyncat UpName
  | UnrecognizedTyVar LowName
  | ExpectedRecursiveType String
  | ExpectingTypeNameOrVar String
  | ExpectedTypeConstructor SExpr
  | UnexpectedLiteral SExpr
  | ConsListsDisallowed SExpr
  deriving (Show)