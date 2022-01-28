{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Language.Nanopass.QQ
  ( deflang
  , defpass
  ) where

import Data.Char
import Language.Nanopass.LangDef
import Prelude hiding (mod)


import Control.Monad (forM)
import Language.Haskell.TH (Q, Dec)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Nanopass.Xlate (mkXlateA)
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
      Nothing -> fail "sexpr syntax error"
    case parseDefBaseOrExt (Just input) sexprs of
      Right (Left def) -> runDefine $ defineLang def
      Right (Right mod) -> runModify mod
      Left err -> fail err
  bad ctx _ = fail $ "`deflang` quasiquoter cannot be used in a " ++ ctx ++ " context,\n\
                     \it can only appear as part of declarations."


defpass :: QuasiQuoter
defpass = QuasiQuoter (bad "expression") (bad "pattern") (bad "type") go
  where
  go input = do
    sexprs <- case Stupid.parse input of
      Just it -> pure it
      Nothing -> fail "sexpr syntax error"
    case parseDefPass sexprs of
      Right (l1Name, l2Name) -> do
        l1 <- reifyLang l1Name
        l2 <- reifyLang l2Name
        mkXlateA l1 l2
      Left err -> fail err
  bad ctx _ = fail $ "`defpass` quasiquoter cannot be used in a " ++ ctx ++ "context,\n\
                     \it can only appear as part of declarations."
  parseDefPass :: [Sexpr String] -> Either String (String, String)
  parseDefPass [Atom l1, Atom ":->", Atom l2]
    | Just l1Name <- fromUpdotname l1
    , Just l2Name <- fromUpdotname l2
      = Right (l1Name, l2Name)
  parseDefPass _ = Left "expecting two language names, separated by :->"

----------------------------------
------ Language Definitions ------
----------------------------------

parseDefBaseOrExt :: Maybe String -> [Sexpr String] -> Either String (Either LangDef LangMod)
parseDefBaseOrExt originalText (langName:Atom ":->":rest) = case rest of
  (extName:rest') -> case rest' of
    (candidateParams:rest'') | Right params <- parseParams candidateParams
      -> Right <$> parseLangMod originalText langName extName params rest''
    _ -> Right <$> parseLangMod originalText langName extName [] rest'
  _ -> Left $ "expecting a new language name"
parseDefBaseOrExt originalText (langName:rest) = case rest of
  (candidateParams:rest') | Right params <- parseParams candidateParams
    -> Left <$> parseLangDef originalText langName params rest'
  _ -> Left <$> parseLangDef originalText langName [] rest
parseDefBaseOrExt _ _ = Left $ "expecting a langauge name"

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

parseLangDef :: Maybe String -> Sexpr String -> [String] -> [Sexpr String] -> Either String LangDef
parseLangDef originalProgram nameExpr langParamReqs syncatExprs = do
  langNameReq <- parseLangName nameExpr
  syncatReqs <- parseSyncat `mapM` syncatExprs
  pure $ LangDef
    { langNameReq
    , langParamReqs
    , syncatReqs
    , originalProgram
    , baseDefdLang = Nothing
    }

parseLangName :: Sexpr String -> Either String String
parseLangName (Atom str) | Just str' <- fromUpname str = pure str'
parseLangName _ = Left "language name must be an UpCase alphanumeric symbol"

parseSyncat :: Sexpr String -> Either String SyncatDef
parseSyncat (Combo "(" (nameExpr:prodExprs)) = do
  sName <- case nameExpr of
    (Atom nameStr) | Just sName <- fromUpname nameStr -> pure sName
    _ -> Left $ concat
      [ "expecting an uppercase name of a syntactic category, got:\n"
      , "  " ++ Stupid.print id nameExpr
      ]
  prods <- parseProd `mapM` prodExprs
  pure $ SyncatDef sName prods
parseSyncat other = Left $ concat
  [ "expecting syntactic category definition:\n"
  , "  (<SyncatName> <production>… )\n"
  , "got:\n:"
  , "  " ++ Stupid.print id other
  ]

parseProd :: Sexpr String -> Either String ProdDef
parseProd (Combo "(" (Atom prodStr:subtermExprs))
  | Just prodName <- fromUpname prodStr = do
    subterms <- parseSubterm `mapM` subtermExprs
    pure $ ProdDef prodName subterms
parseProd other = Left $ concat
  [ "expecting a production definition:\n"
  , "  (<ProductionName> <subterm>… )\n"
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
    = pure $ RecursiveType mutrec
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
    pure $ ListType (TupleType lhs rhs [])
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
  , "  $<SyncatName>\n"
  , "  <TypeCtor>                # == ($<TypeCtor>)\n"
  , "  (<TypeCtor> <type>… )\n"
  , "  (<type> <* | + | ?>… )    # list, nonempty list, and maybe\n"
  , "  {<type> <type> <type>… }  # tuple\n"
  , "  [ <type> :-> <type> ]     # association list\n"
  , "  { <type> :-> <type> }     # ord map\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

---------------------------------
------ Language Extensions ------
---------------------------------

parseLangMod :: Maybe String -> Sexpr String -> Sexpr String -> [String] -> [Sexpr String] -> Either String LangMod
parseLangMod originalModProgram baseExpr newExpr newParamReqs modExprs = do
  baseLangReq <- parseBaseLangName baseExpr
  newLangReq <- parseLangName newExpr
  modss <- parseSyncatMod `mapM` modExprs
  pure $ LangMod
    { baseLangReq
    , newLangReq
    , newParamReqs
    , syncatMods = concat modss
    , originalModProgram
    }

parseBaseLangName :: Sexpr String -> Either String String
parseBaseLangName (Atom str) | Just str' <- fromUpdotname str = pure str'
parseBaseLangName _ = Left "base language name must be a non-empty list of dot-separated UpCase alphanumeric symbol"

parseSyncatMod :: Sexpr String -> Either String [SyncatMod]
parseSyncatMod (Combo "(" (Atom "+":syncatExprs)) = do
  (fmap AddSyncat . parseSyncat) `mapM` syncatExprs
parseSyncatMod (Combo "(" (Atom "-":syncatExprs)) =
  forM syncatExprs $ \case
    (Atom syncatStr) | Just sName <- fromUpname syncatStr -> pure $ DelSyncat sName
    other -> Left $ "expecting the name of a syntactic category, got:\n  " ++ Stupid.print id other
parseSyncatMod (Combo "(" (Atom "*":syncatExprs)) =
  forM syncatExprs $ \case
    (Combo "(" (Atom sStr:pModExprs))
      | Just sName <- fromUpname sStr -> do
        pMods <- parseProdMod `mapM` pModExprs
        pure $ ModProds sName pMods
    other -> Left $ concat
      [ "expecting syntactic category modifier:\n"
      , "  (<SyncatName> <ctor mods>… )\n"
      , "got:\n"
      , "  " ++ Stupid.print id other
      ]
parseSyncatMod other = Left $ concat
  [ "expecting syntactic category modifier batch:\n"
  , "  (+ <syncat modifier>… )\n"
  , "  (* <syncat modifier>… )\n"
  , "  (- <syncat modifier>… )\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

parseProdMod :: Sexpr String -> Either String ProdMod
parseProdMod (Combo "(" (Atom "+":Atom prodStr:subtermExprs))
  | Just prodName <- fromUpname prodStr = do
    subterms <- parseSubterm `mapM` subtermExprs
    pure $ AddProd $ ProdDef prodName subterms
parseProdMod (Combo "(" [Atom "-", Atom prodStr])
  | Just prodName <- fromUpname prodStr = pure $ DelProd prodName
parseProdMod other = Left $ concat
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