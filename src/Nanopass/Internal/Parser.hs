{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | This module contains functions that translate Haskell quasiquotes into
-- internal representations of languages and passes as defined in "Nanopass.Internal.Representation".
-- This is done by first parsing an s-expression with "Text.SExpression",
-- and then recursively recognizing the components of that s-expression.
--
-- The primary entry points are 'parseLanguage' and TODO 'parsePass`.
-- Other recognizers are exported to serve as the promary source for
-- documentation about the grammar used in these quasiquotes.
module Nanopass.Internal.Parser
  (
  -- * Recognizers
    parseLanguage
  -- ** Base Languages
  , parseBaseLanguage
  , parseNonterm
  , parseProduction
  -- ** Language Modification
  , parseLangMod
  , parseNontermsEdit
  , parseProductionsEdit
  -- ** Shared Recognizers
  , parseLangLHS
  , parseNontermBody
  , parseProductionBody
  , parseType
  -- * S-Expressions
  , getSexpr
  , Loc(..)
  , toUpColonName
  -- * Error Reporting
  , Error(..)
  ) where

import Nanopass.Internal.Representation

import Control.Monad (forM)
import Data.Functor ((<&>))
import Text.Megaparsec (runParser',State(..),PosState(..),SourcePos(..),errorBundlePretty)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Pos (defaultTabWidth,mkPos)
import Text.SExpression (SExpr(..),Parser,parseSExpr,def)

import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as P

------------------------------
------ Glue it together ------
------------------------------

-- | @
-- Language ::= \<BaseLang\> | \<LangMod\>
-- @
--
-- * for @BaseLang@, see 'parseBaseLanguage'
-- * for @LangMod@, see 'parseLangMod'
parseLanguage :: (Loc, String) -> Either Error (Either (Language 'Valid) LangMod)
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
--   (\<LangLHS\>        language name and type variables
--       \<string…\>     documentation
--       \<Nonterm…\>)   syntactic categories
-- @
--
-- * for @LangLHS@, see 'parseLangLHS'
-- * for @Nonterm@, see 'parseNonterm'
parseBaseLanguage :: String -> SExpr -> Either Error (Language 'Valid)
parseBaseLanguage originalProgram (List (lhs:rest)) = do
  (name, langParams) <- parseLangLHS lhs
  let langName = ValidName (unDotted name) (TH.mkName $ fromUpName name)
      (docs, nonterms_) = spanDocstrs rest
  nontermList <- parseNonterm `mapM` nonterms_
  let nonterms = Map.fromList $ nontermList <&> \s -> (s.nontermName.base, s)
  pure Language
    { langName
    , langParams = langParams <&> \n -> ValidName n (TH.mkName $ fromLowName n)
    , nonterms
    , originalProgram = Just originalProgram
    , baseDefdLang = Nothing
    }
parseBaseLanguage _ other = Left $ ExpectingLanguage other

-- | @
-- LangLHS ::= \<UpCase\>                 language name, zero type variables
--          |  (\<UpCase\> \<LowCase…\>)    language name, type variables
-- @
--
-- * for @UpCase@, see 'toUpName'
-- * for @LowCase@, see 'toLowName'
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
-- Nonterm ::=
--   (\<UpCase\>             type name
--       \<string…\>         documentation
--       \<Production…\>)    constructor arguments
-- @
--
-- * for @UpCase@, see 'toUpName'
-- * for @Production@, see 'parseProduction'
parseNonterm :: SExpr -> Either Error (Nonterm 'Valid)
parseNonterm (List (Atom str:rest)) = do
  nontermName <- case toUpName str of
    Just name -> pure name
    Nothing -> Left $ ExpectedNontermName (Just $ Atom str)
  parseNontermBody nontermName rest
parseNonterm (List (other:_)) = Left $ ExpectedNontermName (Just other)
parseNonterm other = Left $ ExpectedNonterm other

-- | Separated out from 'parseNonterm' because it is useful in 'parseNontermsEdit' as well.
parseNontermBody :: UpName -> [SExpr] -> Either Error (Nonterm 'Valid)
parseNontermBody nontermName rest = do
  let (docs, prods_) = spanDocstrs rest
  productionList <- parseProduction `mapM` prods_
  let productions = Map.fromList $ productionList <&> \p -> (p.prodName.base, p)
  pure Nonterm
    { nontermName = ValidName nontermName (TH.mkName $ fromUpName nontermName)
    , productions
    }

-- | @
-- Production ::=
--   (\<UpCase\>        constructor name
--       \<string…\>    documentation
--       \<Type…\>)     constructor arguments
-- @
--
-- * for @UpCase@, see 'toUpName'
-- * for @Type@, see 'parseType'
parseProduction :: SExpr -> Either Error (Production 'Valid)
parseProduction (List (Atom ctorStr:rest)) = do
  prodName <- case toUpName ctorStr of
    Just name -> pure name
    Nothing -> Left $ ExpectedConstructorName (Just $ Atom ctorStr)
  parseProductionBody prodName rest
parseProduction other = Left $ ExpectedProduction other

-- | Separated out from 'parseProduction' because it is useful in 'parseProductionsEdit' as well.
parseProductionBody :: UpName -> [SExpr] -> Either Error (Production 'Valid)
parseProductionBody prodName rest = do
  let (docs, args) = spanDocstrs rest
  subterms <- parseType `mapM` args
  pure Production
    { prodName = ValidName prodName (TH.mkName $ fromUpName prodName)
    , subterms
    }

-- | @
-- Type ::= (\'$\' \<UpCase name\>)            non-terminal (language parameters already applied)
--       |  \<lowCase name\>                 type parameter
--       |  \<UpColonName\>                  plain Haskell type (kind *)
--       |  (\<UpColonName\> \<Type…\>)        plain Haskell type application
--       |  (\'?\' \<Type\>)                   Maybe type
--       |  (\'*\' \<Type\>)                   List type
--       |  (\'+\' \<Type\>)                   NonEmpty type
--       |  () | (\'&\')                     unit type
--       |  (\'&\' \<Type\>)                   Only type TODO
--       |  (\'&\' \<Type\> \<Type\> \<Type…\>)    tuple types
-- @
--
--
-- * for @UpCase@, see 'toUpName'
-- * for @LowCase@, see 'toLowName'
-- * for @UpColonCase@, see 'toUpColonName'
parseType :: SExpr -> Either Error (TypeDesc 'Valid)
parseType = \case
  Atom str
    | Just name <- toUpColonName str
      -> pure $ CtorType (ValidName name (TH.mkName $ fromUpDotName name)) []
    | Just name <- toLowName str
      -> pure $ VarType (ValidName name (TH.mkName $ fromLowName name))
    | otherwise -> Left $ ExpectingTypeNameOrVar str
  List [] -> pure UnitType
  List (Atom "$" : rest) -> case rest of
    [Atom str]
      | Just name <- toUpName str -> pure $ RecursiveType name
      | otherwise -> Left $ ExpectedNontermName (Just $ Atom str)
    [x] -> Left $ ExpectedNontermName (Just x)
    (_:x:_) -> Left $ UnexpectedSExprAfterNonterminal x
    [] -> Left $ ExpectedNontermName Nothing
  List [Atom "?", x] -> MaybeType <$> parseType x
  List [Atom "*", x] -> ListType <$> parseType x
  List [Atom "+", x] -> NonEmptyType <$> parseType x
  List (Atom "&" : xs_) -> case xs_ of
    [] -> pure UnitType
    [x] -> parseType x -- TODO Data.Tuple.Only
    (x1:x2:xs) -> do
      t1 <- parseType x1
      t2 <- parseType x2
      ts <- parseType `mapM` xs
      pure $ TupleType t1 t2 ts
  List (x:xs) -> do
    ctor <- case x of
      Atom str | Just name <- toUpColonName str -> pure name
      _ -> Left $ ExpectedTypeConstructor x
    ts <- parseType `mapM` xs
    pure $ CtorType (ValidName ctor (TH.mkName $ fromUpDotName ctor)) ts
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
--   (\<LangLHS\>             new language name and type variables
--         \'from\'           keyword
--         \<UpColon\>        base language name
--       \<string…\>          documentation
--       \<NontermsEdit…\>)   changes to the base language's syntactic categories
-- @
--
-- * for @LangLHS@, see 'parseLangLHS'
-- * for @UpColon@, see 'toUpColonName'
-- * for @NontermsEdit@, see 'parseNontermsEdit'
parseLangMod :: String -> SExpr -> Either Error LangMod
parseLangMod originalModProgram (List (lhs:Atom "from":rest_)) = do
  (newLang, newParams) <- parseLangLHS lhs
  (baseLang, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpColonName str -> pure (name, rest)
    other:_ -> Left $ ExpectingBaseLanguage (Just other)
    _ -> Left $ ExpectingBaseLanguage Nothing
  let (docs, edits_) = spanDocstrs rest
  edits <- parseNontermsEdit `mapM` edits_
  pure LangMod
    { baseLang
    , newLang
    , newParams
    , nontermsEdit = edits
    , originalModProgram = Just originalModProgram
    }
parseLangMod _ other = Left $ ExpectingKwFromAfterLHS other

-- | @
-- NontermsEdit
--   ::= (\'+\'                       add a syntactic category
--           \<UpCase\>                 new non-terminal name
--           \<string…\>                documentation
--           \<Production…\>)           constructors
--    |  (\'-\' \<UpCase\>)             remove a syntactic category by name
--    |  (\'*\'                       modify a syntactic category's productions
--           \<UpCase name\>            name of non-terminal to edit
--           \<ProductionsEdit…\>)      changes to the base language's non-terminal
-- @
--
-- * for @UpCase@, see 'toUpName'
-- * for @Production@, see 'parseProduction'
-- * for @ProductionsEdit@, see 'parseProductionsEdit'
parseNontermsEdit :: SExpr -> Either Error NontermsEdit
parseNontermsEdit (List (Atom "+":rest_)) = do
  (nontermName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedNontermName (Just other)
    [] -> Left $ ExpectedNontermName Nothing
  nonterm <- parseNontermBody nontermName rest
  pure $ AddNonterm (forceUnvalidateNonterm nonterm)
parseNontermsEdit (List (Atom "-":rest_)) = do
  (nontermName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedNontermName (Just other)
    [] -> Left $ ExpectedNontermName Nothing
  case rest of
    [] -> pure ()
    x:_ -> Left $ UnexpectedSExprAfterDelete x
  pure $ DelNonterm nontermName
parseNontermsEdit (List (Atom "*":rest_)) = do
  (nontermName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedNontermName (Just other)
    [] -> Left $ ExpectedNontermName Nothing
  edits <- parseProductionsEdit `mapM` rest
  pure $ ModNonterm nontermName edits
parseNontermsEdit (List (other:_)) = Left $ ExpectingPlusMinusStar other
parseNontermsEdit other = Left $ ExpectingNontermsEdit other

forceUnvalidateNonterm :: Nonterm 'Valid -> Nonterm 'Unvalidated -- DELME
forceUnvalidateNonterm Nonterm{nontermName,productions} = Nonterm
  { nontermName = SourceName nontermName.base
  , productions = forceUnvalidatedProd <$> productions
  }

-- | @
-- ProductionsEdit
--   ::= (\'+\'              add a production
--           \<UpCase\>        new constructor name
--           \<string…\>       documentation
--           \<Type…\>)        constructor arguments
--    |  (\'-\' \<UpCase\>)    remove a production by name
-- @
--
-- * for @UpCase@, see 'toUpName'
-- * for @Type@, see 'parseType'
parseProductionsEdit :: SExpr -> Either Error ProductionsEdit
parseProductionsEdit (List (Atom "+":rest_)) = do
  (prodName, rest) <- case rest_ of
    (Atom str):rest | Just name <- toUpName str -> pure (name, rest)
    other:_ -> Left $ ExpectedConstructorName (Just other)
    [] -> Left $ ExpectedConstructorName Nothing
  prod <- parseProductionBody prodName rest
  pure $ AddProd (forceUnvalidatedProd prod)
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

forceUnvalidatedProd :: Production 'Valid -> Production 'Unvalidated -- DELME
forceUnvalidatedProd Production{prodName,subterms} = Production
  { prodName = SourceName prodName.base
  , subterms = forceUnvalidatedType <$> subterms
  }

forceUnvalidatedType :: TypeDesc 'Valid -> TypeDesc 'Unvalidated -- DELME
forceUnvalidatedType (RecursiveType n) = RecursiveType n
forceUnvalidatedType (VarType n) = VarType (SourceName n.base)
forceUnvalidatedType (CtorType n ts) = CtorType (SourceName n.base) (forceUnvalidatedType <$> ts)
forceUnvalidatedType (ListType t) = ListType (forceUnvalidatedType t)
forceUnvalidatedType (MaybeType t) = MaybeType (forceUnvalidatedType t)
forceUnvalidatedType (NonEmptyType t) = NonEmptyType (forceUnvalidatedType t)
forceUnvalidatedType UnitType = UnitType
forceUnvalidatedType (TupleType t1 t2 ts) = TupleType (forceUnvalidatedType t1) (forceUnvalidatedType t2) (forceUnvalidatedType <$> ts)


---------------------------------
------ Parse S-Expressions ------
---------------------------------

-- | This is a location type that should be sufficient to describe the start of a Template Haskell quasiquote.
-- It is used in 'getSexpr' so that it can report errors from the actual source code location.
data Loc = Loc
  { file :: FilePath
  , line :: Int
  , col :: Int
  }

-- | This serves as an adapter between Template Haskell and whatever s-expression parser I decide to use.
getSexpr :: (Loc, String) -> Either Error SExpr
getSexpr (loc, inp) = case runParser' (sc *> parseSExpr def <* sc) state0 of
    (_, Left err) -> Left . SexprError $ errorBundlePretty err
    (_, Right sexpr) -> Right sexpr
  where
  sc :: Parser ()
  sc = P.space space1 (P.skipLineComment ";") P.empty
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
--
-- That is, accept strings matching @[A-Z][a-zA-Z0-9_]\(:[A-Z][a-zA-Z0-9_])*@,
-- and alter them with @s\/\\.\/:\/@.
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
  | ExpectingNontermsEdit SExpr
  | ExpectingPlusMinus SExpr
  | ExpectingProductionsEdit SExpr
  -- parseNonterm
  | ExpectedNontermName (Maybe SExpr)
  | ExpectedNonterm SExpr
  -- parseProduction
  | ExpectedConstructorName (Maybe SExpr)
  | ExpectedProduction SExpr
  -- parseType
  | UnexpectedSExprAfterNonterminal SExpr
  | ExpectingTypeNameOrVar String
  | ExpectedTypeConstructor SExpr
  | UnexpectedLiteral SExpr
  | ConsListsDisallowed SExpr
  deriving (Show)