{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Language.Nanopass.QQ
  ( deflang
  , defpass
  ) where


import Language.Nanopass.LangDef
import Nanopass.Internal.Representation
import Prelude hiding (mod)


import Control.Monad (forM)
import Data.Functor ((<&>))
import Language.Haskell.TH (Q, Dec)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Nanopass.Xlate (mkXlate)
import Text.Parse.Stupid (Sexpr(..))

import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
import qualified Language.Haskell.TH as TH
import qualified Nanopass.Internal.Parser as Parser
import qualified Text.Parse.Stupid as Stupid
import qualified Text.Pretty.Simple as PP

-- | Define a language, either from scratch or by derivation from an existing language.
-- The syntax is based on s-expressions. Whitespace doesn't matter, and a (full) line can be commented out with a hash (@#@).
-- More details and examples are given in the [readme](https://github.com/edemko/nanopass/blob/master/README.md).
--
-- We embed the syntax of the quasiquoters in a modified form of sexprs which allow---and distinguish between---square and curly brackets alongside round brackets.
-- Atoms are just sequences of characters that don't contain whitespace, though we only recognize a handful of these as valid syntactically.
-- Importantly, we treat symbols differently based on their shape:
--
--   * @UpCamelCase@ is used as in normal Haskell: to identify constructors, both type- and data-
--   * @$Name@ is used for recursive references to syntactic categories
--   * @lowerCamel@ is used for language parameters and the names of terms
--   * @DotSeparated.UpCamelCase@ is used to qualify the names of languages and types.
--   * a handful of operators are used
-- 
-- Since the syntax is based on s-expressions, we use [Scheme's entry format](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-4.html#%_sec_1.3.3) conventions for describing the syntax.
-- Importantly, we syntactic variables are enclosed in @⟨angle brackets⟩@, and ellipsis @⟨thing⟩…@ indicate zero or more repetitions of @⟨thing⟩@.
-- Round, square, and curly brackets, as well as question mark, asterisk, and so on have no special meaning: they only denote themselves.
--
-- >  langdef
-- >    ::= ⟨language definition⟩
-- >     |  ⟨language modification⟩
-- >  
-- >  language definition
-- >    ::= ⟨UpName⟩ ( ⟨lowName⟩… ) ⟨syntactic category⟩…
-- >    ::= ⟨UpName⟩ ⟨syntactic category⟩…
-- >  
-- >  language modification
-- >    ::= ⟨Up.Name⟩ :-> ⟨UpName⟩ ( ⟨lowName⟩… ) ⟨syntactic category modifier⟩…
-- >     |  ⟨Up.Name⟩ :-> ⟨UpName⟩ ⟨syntactic category modifier⟩…
-- >  
-- >  syntactic category ::= ( ⟨UpName⟩ ⟨production⟩… )
-- >  production ::= ( ⟨UpName⟩ ⟨subterm⟩… )
-- >  subterm
-- >    ::= { ⟨lowName⟩ ⟨type⟩ }
-- >     |  ⟨type⟩
-- >  
-- >  syntactic category modifier
-- >    ::= ( + ⟨syntactic category⟩… )
-- >     |  ( - ⟨UpName⟩… )
-- >     |  ( * ⟨UpName⟩ ⟨production modifier⟩… )
-- >     |  ( * ( ⟨UpName⟩ ⟨production modifier⟩… )… )
-- >  production modifier
-- >    ::= ( + ⟨UpName⟩ ⟨subterm⟩… )
-- >     |  ( - ⟨UpName⟩ )
-- >  
-- >  type
-- >    ::= $⟨UpName⟩                               # reference a syntactic category
-- >     |  ⟨lowName⟩                               # type parameter
-- >     |  ( ⟨Up.Name⟩ ⟨type⟩… )                   # apply a Haskell Type constructor to arguments
-- >     |  ⟨Up.Name⟩                               # same as: (⟨Up.Name⟩)
-- >     |  ( ⟨type⟩ ⟨type operator⟩… )             # apply common type operators (left-associative)
-- >     |  ( ⟨Up.Name⟩ ⟨type⟩… ⟨type operator⟩… )  # same as: ((⟨UpName⟩ ⟨type⟩…) ⟨type operator⟩…)
-- >     |  { ⟨type⟩ ⟨type⟩ ⟨type⟩… }               # tuple type
-- >     |  [ ⟨type⟩ :-> ⟨type⟩ ]                   # association list: ({⟨type⟩ ⟨type⟩} *)
-- >     |  { ⟨type⟩ :-> ⟨type⟩ }                   # Data.Map
-- >  
-- >  type operator
-- >    ::= *  # []
-- >     |  +  # NonEmpty
-- >     |  ?  # Maybe
deflang :: QuasiQuoter
deflang = QuasiQuoter (bad "expression") (bad "pattern") (bad "type") go
  where
  go :: String -> Q [Dec]
  go input = do
    loc <- TH.location <&> \l -> Parser.Loc
        { Parser.file = l.loc_filename
        , Parser.line = fst l.loc_start
        , Parser.col = snd l.loc_start
        }
    case Parser.parseLanguage (loc, input) of
      (Right (Left def)) -> runDefine $ defineLang def
      (Right (Right mod)) -> runModify mod
      Left err -> fail $ (LT.unpack . PP.pShow) err -- TODO
  bad ctx _ = fail $ "`deflang` quasiquoter cannot be used in " ++ ctx ++ " context,\n\
                     \it can only appear as part of declarations."

-- | Define automatic translation between two langauges.
-- This creates an @Xlate@ type and the @descend\<Syntactic Category\>@ family of functions,
--   as well as pure variants (@XlateI@ and @descend\<Syntactic Category\>I@) and a lifting function @idXlate@.
-- A translation function is generated for each syntactic category with the same name in both source and target languages.
-- At the moment, there is no provision for altering the name of the type or translation function(s),
--   but I expect you'll only want to define one translation per module.
--
-- The @Xlate@ type takes all the parameters from both languages (de-duplicating parameters of the same name),
--   as well as an additional type parameter, which is the functor @f@ under which the translation occurs.
--
-- The type of a @descend\<Syntactic Category\>@ function is
--   @Xlate f → σ → f σ'@.
--
-- If a production in the source language has subterms @τ₁ … τₙ@ and is part of the syntactic category @σ@,
--   then a hole member is a function of type @τ₁ → … τₙ → f σ'@, where @σ'@ is the corresponding syntactic category in the target language.
-- Essentially, you get access all the subterms, and can use the 'Applicative' to generate a target term as long as you don't cross syntactic categories.
--
-- If a source language has syntactic category @σ@ with the same name as the target's syntactic category @σ'@,
--   then an override member is a function of type @σ → 'Maybe' (f σ')@.
-- If an override returns 'Nothing', then the automatic translation will be used,
--   otherwise the automatic translation is ignored in favor of the result under the 'Just'.
--
-- The pure variants have the same form as the 'Applicative' ones, but:
--
--   * @XlateI@ is not parameterized by @f@, nor are the types of its members,
--   * the members of @XlateI@ are suffixed with the letter @I@, and
--   * the types of the @descend\<Syntactic Category\>I@ functions are not parameterzed by @f@.
--
-- The @idXlate@ function is used by Nanopass to translate @XlateI@ values into @Xlate@ values.
-- This is done so that the same code paths can be used for both pure and 'Applicative' translations.
-- Under the hood, this is done with appropriate wrapping/unwrapping of v'Data.Functor.Identity.Identity', which is a no-op.
--
-- None of the functions defined by this quasiquoter need to be expoted for Nanopass to function.
-- I expect you will not export any of these definitions directly, but instead wrap them into a complete pass, and only export that pass.
--
-- More details and examples are given in the [readme](https://github.com/edemko/nanopass/blob/master/README.md).
--
-- The syntax is:
--
-- >  ⟨Up.Name⟩ :-> ⟨Up.Name⟩
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
        mkXlate l1 l2
      Left err -> fail err
  bad ctx _ = fail $ "`defpass` quasiquoter cannot be used in a " ++ ctx ++ "context,\n\
                     \it can only appear as part of declarations."
  parseDefPass :: [Sexpr String] -> Either String (UpDotName, UpDotName)
  parseDefPass [Atom l1, Atom ":->", Atom l2]
    | Just l1Name <- toUpDotName l1
    , Just l2Name <- toUpDotName l2
      = Right (l1Name, l2Name)
  parseDefPass _ = Left "expecting two language names, separated by :->"

----------------------------------
------ Language Definitions ------
----------------------------------

parseDefBaseOrExt :: Maybe String -> [Sexpr String] -> Either String (Either Language LangMod)
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

parseParams :: Sexpr String -> Either String [LowName]
parseParams (Combo "(" params) = parseParam `mapM` params
  where
  parseParam (Atom str) | Just param <- toLowName str = Right param
  parseParam other = Left $ "expecting type parameter (lowercase symbol), got: " ++ show other
parseParams other = Left $ concat
  [ "expecting parameter list:\n"
  , "  (<lowercase name…> )\n"
  , "got:\n"
  , "  " ++ show other
  ]

parseLangDef :: Maybe String -> Sexpr String -> [LowName] -> [Sexpr String] -> Either String Language
parseLangDef originalProgram nameExpr langParams syncatExprs = do
  lName <- parseLangName nameExpr
  syncats <- parseSyncat `mapM` syncatExprs
  pure $ Language
    { langName = unDotted lName
    , langNameTH = TH.mkName $ fromUpName lName
    , langParams
    , syncats = Map.fromList $ syncats <&> \s -> (s.syncatName, s)
    , originalProgram
    , baseDefdLang = Nothing
    }

parseLangName :: Sexpr String -> Either String UpName
parseLangName (Atom str) | Just str' <- toUpName str = pure str'
parseLangName _ = Left "language name must be an UpCase alphanumeric symbol"

parseSyncat :: Sexpr String -> Either String Syncat
parseSyncat (Combo "(" (nameExpr:prodExprs)) = do
  sName <- case nameExpr of
    (Atom nameStr) | Just sName <- toUpName nameStr -> pure sName
    _ -> Left $ concat
      [ "expecting an uppercase name of a syntactic category, got:\n"
      , "  " ++ Stupid.print id nameExpr
      ]
  prods <- parseProd `mapM` prodExprs
  pure $ Syncat sName (TH.mkName $ fromUpName sName) (Map.fromList $ prods <&> \p -> (p.prodName, p))
parseSyncat other = Left $ concat
  [ "expecting syntactic category definition:\n"
  , "  (<SyncatName> <production>… )\n"
  , "got:\n:"
  , "  " ++ Stupid.print id other
  ]

parseProd :: Sexpr String -> Either String Production
parseProd (Combo "(" (Atom prodStr:subtermExprs))
  | Just prodName <- toUpName prodStr = do
    subterms <- parseSubterm `mapM` subtermExprs
    pure $ Production
      { prodName
      , prodNameTH = TH.mkName $ fromUpName prodName
      , subterms
      }
parseProd other = Left $ concat
  [ "expecting a production definition:\n"
  , "  (<ProductionName> <subterm>… )\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

parseSubterm :: Sexpr String -> Either String TypeDesc
parseSubterm (Combo "{" [Atom fieldStr, typeExpr])
  | Just fieldName <- toLowName fieldStr = parseType typeExpr
parseSubterm typeEexpr = case parseType typeEexpr of
  Right typeDesc -> pure typeDesc
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
  , Just mutrec <- toUpName str'
    = pure $ RecursiveType mutrec
  | Just tyvar <- toLowName str
    = pure $ VarType (TH.mkName $ fromLowName tyvar)
  | Just ctorName <- toUpDotName str = pure $ CtorType (TH.mkName $ fromUpDotName ctorName) []
parseType (Combo "(" subexprs)
  | Just (innerExpr, modifier) <- fromShortcut subexprs = do
      innerType <- parseType innerExpr
      pure $ modifier innerType
  | Just (tycon, argExprs) <- fromTycon subexprs = do
    args <- parseType `mapM` argExprs
    pure $ CtorType (TH.mkName $ fromUpDotName tycon) args
parseType (Combo "[" subexprs)
  | Just (lhsExpr, rhsExpr) <- fromMapType subexprs = do
    lhs <- parseType lhsExpr
    rhs <- parseType rhsExpr
    pure $ ListType (TupleType lhs rhs [])
parseType (Combo "{" subexprs) =
  parseType `mapM` subexprs >>= \case
    (t1:t2:ts) -> pure $ TupleType t1 t2 ts
    _ -> Left $ concat
      [ "expecting two or more types as part of a tuple, got:\n"
      , unlines $ Stupid.print id <$> subexprs
      ]
parseType other = Left $ concat
  [ "expecting type description, one of:\n"
  , "  $<SyncatName>\n"
  , "  <typeParam>\n"
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

parseLangMod :: Maybe String -> Sexpr String -> Sexpr String -> [LowName] -> [Sexpr String] -> Either String LangMod
parseLangMod originalModProgram baseExpr newExpr newParams modExprs = do
  baseLang <- parseBaseLangName baseExpr
  newLang <- parseLangName newExpr
  modss <- parseSyncatsEdit `mapM` modExprs
  pure $ LangMod
    { baseLang
    , newLang
    , newParams
    , syncatsEdit = concat modss
    , originalModProgram
    }

parseBaseLangName :: Sexpr String -> Either String UpDotName
parseBaseLangName (Atom str) | Just str' <- toUpDotName str = pure str'
parseBaseLangName _ = Left "base language name must be a non-empty list of dot-separated UpCase alphanumeric symbol"

parseSyncatsEdit :: Sexpr String -> Either String [SyncatsEdit]
parseSyncatsEdit (Combo "(" (Atom "+":syncatExprs)) = do
  (fmap AddSyncat . parseSyncat) `mapM` syncatExprs
parseSyncatsEdit (Combo "(" (Atom "-":syncatExprs)) =
  forM syncatExprs $ \case
    (Atom syncatStr) | Just sName <- toUpName syncatStr -> pure $ DelSyncat sName
    other -> Left $ "expecting the name of a syntactic category, got:\n  " ++ Stupid.print id other
parseSyncatsEdit (Combo "(" (Atom "*":Atom sStr:pModExprs))
  | Just sName <- toUpName sStr = do
    pMods <- parseProdMod `mapM` pModExprs
    pure [ModSyncat sName pMods]
  | otherwise = Left $ concat
      [ "expecting syntactic category name"
      , ", got: ", show sStr
      ]
parseSyncatsEdit (Combo "(" (Atom "*":syncatExprs)) =
  forM syncatExprs $ \case
    (Combo "(" (Atom sStr:pModExprs))
      | Just sName <- toUpName sStr -> do
        pMods <- parseProdMod `mapM` pModExprs
        pure $ ModSyncat sName pMods
    other -> Left $ concat
      [ "expecting syntactic category modifier:\n"
      , "  (<SyncatName> <ctor mods>… )\n"
      , "got:\n"
      , "  ", Stupid.print id other
      ]
parseSyncatsEdit other = Left $ concat
  [ "expecting syntactic category modifier batch:\n"
  , "  (+ <syncat modifier>… )\n"
  , "  (* <syncat modifier>… )\n"
  , "  (- <syncat modifier>… )\n"
  , "got:\n"
  , "  " ++ Stupid.print id other
  ]

parseProdMod :: Sexpr String -> Either String ProductionsEdit
parseProdMod (Combo "(" (Atom "+":Atom prodStr:subtermExprs))
  | Just prodName <- toUpName prodStr = do
    subterms <- parseSubterm `mapM` subtermExprs
    pure $ AddProd $ Production
      { prodName
      , prodNameTH = TH.mkName $ fromUpName prodName
      , subterms
      }
parseProdMod (Combo "(" [Atom "-", Atom prodStr])
  | Just prodName <- toUpName prodStr = pure $ DelProd prodName
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

fromTycon :: [Sexpr String] -> Maybe (UpDotName, [Sexpr String])
fromTycon (Atom tyconName : argExprs) = do
  tycon <- toUpDotName tyconName
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

