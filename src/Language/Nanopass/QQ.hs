{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Language.Nanopass.QQ where

import Data.Char
import Language.Nanopass.LangDef
import Prelude hiding (mod)

import Control.Monad (forM)
import Language.Haskell.TH (Q, Dec)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Parse.Stupid (Sexpr(..))

import qualified Language.Haskell.TH as TH
import qualified Text.Parse.Stupid as Stupid

deflang :: QuasiQuoter
deflang = QuasiQuoter (bad "expression") (bad "pattern") (bad "type") go
  where
  go :: String -> Q [Dec]
  go input = do
    sexprs <- case Stupid.parse input of
      Just it -> pure it
      Nothing -> fail "syntax error"
    case parseDefBaseOrExt sexprs of
      Right (Left def) -> runDefine $ defineLang def
      Right (Right mod) -> runModify mod
      Left err -> fail err
  bad ctx _ = fail $ "`deflang` quasiquoter cannot be used in an " ++ ctx ++ " context,\n\
                     \it can only appear as part of declarations."

----------------------------------
------ Language Definitions ------
----------------------------------

parseDefBaseOrExt :: [Sexpr String] -> Either String (Either LangDef LangMod)
parseDefBaseOrExt (langName:Atom ":->":rest) = case rest of
  (extName:rest') -> case rest' of
    (candidateParams:rest'') | Right params <- parseParams candidateParams
      -> Right <$> parseLangMod langName extName params rest''
    _ -> Right <$> parseLangMod langName extName [] rest'
  _ -> Left $ "expecting a new language name"
parseDefBaseOrExt (langName:rest) = case rest of
  (candidateParams:rest') | Right params <- parseParams candidateParams
    -> Left <$> parseLangDef langName params rest'
  _ -> Left <$> parseLangDef langName [] rest
parseDefBaseOrExt _ = Left $ "expecting a langauge name"

parseParams :: Sexpr String -> Either String [String]
parseParams (Combo "(" params) = parseParam `mapM` params
  where
  parseParam (Atom str) | Just param <- fromLowername str = Right param
  parseParam other = Left $ "expecting type parameter (lowercase symbol), got: " ++ show other
parseParams other = Left $ concat
  [ "expecting parameter list:\n"
  , "  (<lowercase name…> )\n"
  , "got:\n"
  , "  " ++ show other
  ]

parseLangDef :: Sexpr String -> [String] -> [Sexpr String] -> Either String LangDef
parseLangDef nameExpr langParamReqs grammarExprs = do
  langNameReq <- parseLangName nameExpr
  grammarReqs <- parseGrammar `mapM` grammarExprs
  pure $ LangDef
    { langNameReq
    , langParamReqs
    , grammarReqs
    }

parseLangName :: Sexpr String -> Either String String
parseLangName (Atom str) | Just str' <- fromUpname str = pure str'
parseLangName _ = Left "language name must be an UpCase alphanumeric symbol"

parseGrammar :: Sexpr String -> Either String GrammarDef
parseGrammar (Combo "(" (nameExpr:ctorExprs)) = do
  gName <- case nameExpr of
    (Atom nameStr) | Just gName <- fromUpname nameStr -> pure gName
    _ -> Left $ concat
      [ "expecting an uppercase grammar name, got:\n"
      , "  " ++ Stupid.print id nameExpr
      ]
  ctors <- parseCtor `mapM` ctorExprs
  pure $ GrammarDef gName ctors
parseGrammar other = Left $ concat
  [ "expecting grammar definition:\n"
  , "  (<TypeName> <ctor>… )\n"
  , "got:\n:"
  , "  " ++ Stupid.print id other
  ]

parseCtor :: Sexpr String -> Either String CtorDef
parseCtor (Combo "(" (Atom ctorStr:subtermExprs))
  | Just ctorName <- fromUpname ctorStr = do
    subterms <- parseSubterm `mapM` subtermExprs
    pure $ CtorDef ctorName subterms
parseCtor other = Left $ concat
  [ "expecting a term contructor definition:\n"
  , "  (<CtorName> <subterm>… )\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

parseSubterm :: Sexpr String -> Either String SubtermDef
parseSubterm (Combo "{" [Atom fieldStr, typeExpr])
  | Just fieldName <- fromLowername fieldStr = do
    typeDesc <- parseType typeExpr
    pure $ SubtermDef (Just fieldName) typeDesc
parseSubterm typeEexpr = case parseType typeEexpr of
  Right typeDesc -> pure $ SubtermDef Nothing typeDesc
  Left errTy -> Left $ concat
    [ "expecting a subterm definition:\n"
    , "     {<fieldName> <type>}\n"
    , "  or <type>\n"
    , "but parsing <type> failed:\n"
    , unlines . fmap ("  "++) . lines $ errTy
    ]

parseType :: Sexpr String -> Either String TypeDesc
parseType (Atom str)
  | '$':str' <- str
  , Just mutrec <- fromUpname str'
    = pure $ GrammarType mutrec
  | Just tyvar <- fromLowername str
    = pure $ VarType (TH.mkName tyvar)
  | Just ctorName <- fromUpdotname str = pure $ CtorType (TH.mkName ctorName) []
parseType (Combo "(" subexprs)
  | Just (innerExpr, modifier) <- fromShortcut subexprs = do
      innerType <- parseType innerExpr
      pure $ modifier innerType
  | Just (tycon, argExprs) <- fromTycon subexprs = do
    args <- parseType `mapM` argExprs
    pure $ CtorType (TH.mkName tycon) args
parseType (Combo "[" subexprs)
  | Just (lhsExpr, rhsExpr) <- fromMapType subexprs = do
    lhs <- parseType lhsExpr
    rhs <- parseType rhsExpr
    pure $ AlistType lhs rhs
parseType (Combo "{" subexprs)
  | Just (lhsExpr, rhsExpr) <- fromMapType subexprs = do
    lhs <- parseType lhsExpr
    rhs <- parseType rhsExpr
    pure $ MapType lhs rhs
  | otherwise = parseType `mapM` subexprs >>= \case
    (t1:t2:ts) -> pure $ TupleType t1 t2 ts
    _ -> Left $ concat
      [ "expecting two or more types as part of a tuple, got:\n"
      , unlines $ Stupid.print id <$> subexprs
      ]
parseType other = Left $ concat
  [ "expecting type description, one of:\n"
  , "  <metavar>\n"
  , "  <TypeCtor>                # == (<TypeCtor>)\n"
  , "  (<TypeCtor> <type>… )\n"
  , "  (<type> <* | + | ?>…)     # list, nonempty list, and maybe\n"
  , "  {<type> <type> <type>… }  # tuple\n"
  , "  [ <type> :-> <type> ]     # association list\n"
  , "  { <type> :-> <type> }     # ord map\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

---------------------------------
------ Language Extensions ------
---------------------------------

parseLangMod :: Sexpr String -> Sexpr String -> [String] -> [Sexpr String] -> Either String LangMod
parseLangMod baseExpr newExpr newParams modExprs = do
  baseLang <- parseBaseLangName baseExpr
  newLang <- parseLangName newExpr
  modss <- parseGrammarMod `mapM` modExprs
  pure $ LangMod
    { baseLang
    , newLang
    , newParams
    , grammarMods = concat modss
    } 

parseBaseLangName :: Sexpr String -> Either String String
parseBaseLangName (Atom str) | Just str' <- fromUpdotname str = pure str'
parseBaseLangName _ = Left "base language name must be a non-empty list of dot-separated UpCase alphanumeric symbol"

parseGrammarMod :: Sexpr String -> Either String [GrammarMod]
parseGrammarMod (Combo "(" (Atom "+":grammarExprs)) = do
  (fmap AddGrammar . parseGrammar) `mapM` grammarExprs
parseGrammarMod (Combo "(" (Atom "-":grammarExprs)) =
  forM grammarExprs $ \case
    (Atom grammarStr) | Just gName <- fromUpname grammarStr -> pure $ DelGrammar gName
    other -> Left $ "expecting grammar name, got:\n  " ++ Stupid.print id other
parseGrammarMod (Combo "(" (Atom "*":grammarExprs)) =
  forM grammarExprs $ \case
    (Combo "(" (Atom gStr:cModExprs))
      | Just gName <- fromUpname gStr -> do
        cMods <- parseCtorMod `mapM` cModExprs
        pure $ ModCtors gName cMods
    other -> Left $ concat
      [ "expecting grammar modifier:\n"
      , "  (<TypeName> <ctor mods>… )\n"
      , "got:\n"
      , "  " ++ Stupid.print id other
      ]
parseGrammarMod other = Left $ concat
  [ "expecting grammar modifier batch:\n"
  , "  (+ <grammar modifier>… )\n"
  , "  (* <grammar modifier>… )\n"
  , "  (- <grammar modifier>… )\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

parseCtorMod :: Sexpr String -> Either String CtorMod
parseCtorMod (Combo "(" (Atom "+":Atom ctorStr:subtermExprs))
  | Just ctorName <- fromUpname ctorStr = do
    subterms <- parseSubterm `mapM` subtermExprs
    pure $ AddCtor $ CtorDef ctorName subterms
parseCtorMod (Combo "(" [Atom "-", Atom ctorStr])
  | Just ctorName <- fromUpname ctorStr = pure $ DelCtor ctorName
parseCtorMod other = Left $ concat
  [ "expecting a contructor modifier:\n"
  , "  (+ <CtorName> <subterm>… )\n"
  , "  (- <CtorName>)\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

-----------------------------------
------ Pattern Match Helpers ------
-----------------------------------

fromTycon :: [Sexpr String] -> Maybe (String, [Sexpr String])
fromTycon (Atom tyconName : argExprs) = do
  tycon <- fromUpdotname tyconName
  pure (tycon, argExprs)
fromTycon _ = Nothing

fromShortcut :: [Sexpr String] -> Maybe (Sexpr String, TypeDesc -> TypeDesc)
fromShortcut exprs0 = case reverse exprs0 of
  yes@(Atom sym:_)
    | sym `elem` (fst <$> shortcuts) -> loop yes
  _ -> Nothing
  where
  loop (Atom sym : rest)
    | Just f' <- lookup sym shortcuts = do
      (inner, f) <- loop rest
      pure (inner, f' . f)
  loop [inner] = pure (inner, id) -- NOTE this is a separate base case b/c we don't want to wrap a metavar in parens
  loop inners@(_:_) = pure (Combo "(" (reverse inners), id)
  loop [] = errorWithoutStackTrace "internal nanopass error in fromShortcut"
  shortcuts =
    [ ("*", ListType)
    , ("+", NonEmptyType)
    , ("?", MaybeType)
    ]

fromMapType :: [Sexpr String] -> Maybe (Sexpr String, Sexpr String)
fromMapType exprs = case break isArrow exprs of
  ([], _) -> Nothing
  (_, []) -> Nothing
  (_, [_]) -> Nothing
  (lhs, _:rhs) ->
    let l = case lhs of { [it] -> it ; _ -> Combo "(" lhs }
        r = case rhs of { [it] -> it ; _ -> Combo "(" rhs }
     in Just (l, r)
  where
  isArrow (Atom ":->") = True
  isArrow _ = False

fromUpdotname :: String -> Maybe String
fromUpdotname inp0 = loop inp0
  where
  loop inp = case break (== '.') inp of
    ([], _) -> Nothing -- no leading dot (or empty string)
    (_, ".") -> Nothing -- no trailing dot
    (_, []) -> Just inp0 -- no more dots
    (_, _:rest) -> loop rest


fromUpname :: String -> Maybe String
fromUpname (c:cs) | isUpper c && all isAlphaNumderscore cs = Just (c:cs)
fromUpname _ = Nothing

fromLowername :: String -> Maybe String
fromLowername (c:cs) | isLower c && all isAlphaNumderscore cs = Just (c:cs)
fromLowername _ = Nothing

isAlphaNumderscore :: Char -> Bool
isAlphaNumderscore c = isAlphaNum c || c == '_'