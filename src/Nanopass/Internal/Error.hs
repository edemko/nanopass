module Nanopass.Internal.Error
  ( Error(..)
  ) where

import Nanopass.Internal.Representation (LowName,UpName,UpDotName)
import Text.SExpression (SExpr)

data Error
  = SExprError String
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
  | ExpectingTypeNameOrVar String
  | ExpectedTypeConstructor SExpr
  | UnexpectedLiteral SExpr
  | ConsListsDisallowed SExpr
  | UnexpectedTypeApplicationstoRecursiveType UpName
  -- validation
  | DuplicateLanguageParams [LowName]
  | UnrecognizedNonterm UpName
  | UnrecognizedTypeVariable LowName
  -- modification
  | IllegalNontermAddedAlsoDeleted UpName -- ^ nonterm was both added and deleted by a language modifier
  | IllegalNontermModificationAlsoAdded UpName -- ^ nonterm was both added and modified by a language modifier
  | IllegalNontermModificationAlsoDeleted UpName -- ^ nonterm was both modified and deleted by a language modifier
  | IllegalNontermAdded UpName -- ^ nonterminal was already present in base language
  | IllegalNontermModified UpName -- ^ nonterminal was not present in base language
  | IllegalNontermDeleted UpName -- ^ nonterminal was not present in base language
  | DuplicateNontermMods [UpName]
  | IllegalProductionAdded UpName -- ^ production was already present in base non-terminal
  | IllegalProductionDeleted UpName -- ^ production was not present in base non-terminal
  -- passes
  | MissingFromTo SExpr
  | UnexpectedSExprAfterPass UpDotName UpDotName
  | ExpectedUpDotNameAfterFrom SExpr
  | ExpectedUpDotNameAfterTo SExpr
  deriving (Show)