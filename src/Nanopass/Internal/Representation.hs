{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module holds type definitions that describe the internal
-- representation of language syntaxen as understood by nanopass.
module Nanopass.Internal.Representation
  (
  -- * Types for Base Languages
  -- $ir
    Language(..)
  , LanguageInfo(..)
  , Nonterm(..)
  , Production(..)
  , TypeDesc(..)
  -- * Types for Modifying Manguages
  , LangMod(..)
  , NontermsEdit(..)
  , ProductionsEdit(..)
  -- * Types for Passes
  , Pass(..)
  -- * Helper Types
  , UpName, toUpName, fromUpName
  , LowName, toLowName, fromLowName
  , UpDotName, toUpDotName, fromUpDotName, splitUpDotName
  , unDotted, upDotQualifier, upDotBase, upDotChBase
  , Name(..), Validate(..)
  ) where

import Data.Char (isLower,isUpper,isAlphaNum)
import Data.List (intercalate)
import Data.Map (Map)
import GHC.Records (HasField(..))

import qualified Language.Haskell.TH as TH

------ Names ------

-- | Strings matching @[A-Z][a-zA-Z0-9_]@
newtype UpName = UpName String
  deriving (Show,Eq,Ord)

-- | Introduction form for 'UpName'
toUpName :: String -> Maybe UpName
toUpName (c:cs) | isUpper c && all isAlphaNum cs = Just $ UpName (c:cs)
toUpName _ = Nothing

-- | Elimination form for 'UpName'
fromUpName :: UpName -> String
fromUpName (UpName str) = str

-- | Strings matching @[a-z][a-zA-Z0-9_]@
newtype LowName = LowName String
  deriving (Show,Eq,Ord)

-- | Introduction form for 'LowName'
toLowName :: String -> Maybe LowName
toLowName (c:cs) | isLower c && all isAlphaNum cs = Just $ LowName (c:cs)
toLowName _ = Nothing

-- | Elimination form for 'LowName'
fromLowName :: LowName -> String
fromLowName (LowName str) = str

-- | Strings matching @[A-Z][a-zA-Z0-9_]\(:[A-Z][a-zA-Z0-9_])*@
data UpDotName = UpDotName [UpName] UpName
  deriving (Show,Eq,Ord)

-- | Introduction form for 'UpDotName'
toUpDotName :: String -> Maybe UpDotName
toUpDotName = loop []
  where
  loop acc inp = case break (== '.') inp of
    ([], _) -> Nothing -- no leading dot, double dot, or empty string allowed
    (_, ".") -> Nothing -- no trailing dot allowed
    (str, []) -> do -- no more dots
      endName <- toUpName str
      pure $ UpDotName (reverse acc) endName
    (str, _:rest) -> do
      qual <- toUpName str
      loop (qual:acc) rest

-- | Elimination form for 'UpDotName'
fromUpDotName :: UpDotName -> String
fromUpDotName (UpDotName strs str) = intercalate "." $ fromUpName <$> (strs ++ [str])

-- | Conversion from 'UpName'
unDotted :: UpName -> UpDotName
unDotted x = UpDotName [] x

-- | Get the parts of a dotted name that come before the last dot
upDotQualifier :: UpDotName -> [UpName]
upDotQualifier (UpDotName xs _) = xs

-- | Get the last part of a dotted name
upDotBase :: UpDotName -> UpName
upDotBase (UpDotName _ x) = x

-- | Create a dotted name identical to the first, but with the last part replaced
upDotChBase :: UpDotName -> UpName -> UpDotName
upDotChBase (UpDotName xs _) y = UpDotName xs y

-- | Get the last part of a dotted name and it's prefix
splitUpDotName :: UpDotName -> ([UpName], UpName)
splitUpDotName (UpDotName xs x) = (xs, x)

data Validate = Valid | Unvalidated
data Name v n where
  SourceName :: { name_ :: n } -> Name 'Unvalidated n
  ValidName :: { base_ :: n, th :: TH.Name } -> Name 'Valid n
deriving instance (Show n) => Show (Name v n)
deriving instance (Eq n) => Eq (Name v n)
deriving instance (Ord n) => Ord (Name v n)

instance HasField "name" (Name v n) n where
  getField (SourceName n) = n
  getField (ValidName n _) = n

----------------------------
------ Base Languages ------
----------------------------

-- $ir
--
-- The types 'Language', 'Nonterm', 'Production' mediate between Haskell and the theory of context-free grammars (CFGs).
-- Each of them is an intermediate representation that can be seen from two perspectives:
--
-- * What Haskell concept do they map to?
-- * What CFG concept do they map to?
--
-- We use something like usual, minimal definition of a CFG as a 4-tuple G = (V, Σ, R, S) where
--
-- 1. V is a set of non-terminals (named by 'nontermName')
-- 2. Σ is a set of terminals (which are just ordinary Haskell data types)
-- 3. R is a relation in V × (V ∪ Σ)*. Members of this relation are called rewrite rules (and map to the arguments of a Haskell data constructor).
-- 4. S is the start symbol, though it is not used by nanopass.

-- | This attributes a name to a set of grammatical types.
-- Languages can have different names in different contexts;
-- importantly, they must not be qualified when defining, but they may need to be dotted when referring to a language from another module.
data Language v n = Language
  { langName :: Name v n
  , langInfo :: LanguageInfo v
  }
  deriving(Show)

-- | Seen as a Haskell entity, each 'Language' is a set of mutually-recursive types.
-- Seen from the persepctive of a CFG, each of these types is a non-terminal used to define the abstract grammar of a language.
--
-- See 'Language' for attributing a name to a set of these types.
data LanguageInfo v = LanguageInfo
  { langParams :: ![Name v LowName]
  -- ^ type parameters; these apply to each of the 'Nonterm' types
  , nonterms :: !(Map UpName (Nonterm v)) -- TODO make this a list
  , originalProgram :: !(Maybe String)
  , baseDefdLang :: !(Maybe (Language 'Valid UpDotName))
  }
  deriving(Show)

-- | Seen as a Haskell entity, each 'Nonterm' is a single type with some number of constructors.
-- Seem from the perspective of a CFG, each 'Nonterm' is… well, a non-terminal symbol.
--
-- 'Nonterm's are the primary constituent of a 'Language'.
data Nonterm v = Nonterm
  { nontermName :: !(Name v UpName)
  , productions :: !(Map UpName (Production v))
  }
  deriving(Show)

-- | Seen as a Haskell entity, each 'Production' maps to a constructor for a 'Nonterm' data type.
-- Seen from the perspective of a CFG, each 'Production' maps to a single rewrite rule.
--
-- 'Production's are the primary constituent of 'Nonterm's.
data Production v = Production
  { prodName :: !(Name v UpName)
  , subterms :: ![TypeDesc v]
  }
  deriving(Show)

-- | Seen as a Haskell entity, a 'TypeDesc' gives the type of an argument of a constructor ('Production').
-- Seen from the perspective of a CFG, each 'TypeDesc' is a symbol (terminal or non-terminal) on the right-hand side of a rewrite rule.
--
-- 'TypeDesc's are the primary constituent of 'Production's.
data TypeDesc v
  = RecursiveType UpName
  -- ^ a non-terminal symbol/recursive use of a 'Nonterm' type
  --
  -- These types need not be applied to any arguments, the language 'langParams' get auromatically applied.
  | VarType (Name v LowName)
  -- ^ allows the use of 'langParams' as terminal symbols
  | CtorType (Name v UpDotName) [TypeDesc v]
  -- ^ allows the use of plain (not defined by nanopass) Haskell types,
  -- either as terminal symbols, or as combinators over non-terminal and terminal symbols
  | ListType (TypeDesc v) -- because otherwise, you'd have to always be saying `type List a = [a]`
  -- ^ nanopass has built-in knowledge of lists, so they are represented specially as opposed to with 'CtorType'
  | MaybeType (TypeDesc v)
  -- ^ nanopass has built-in knowledge of optionals, so they are represented specially as opposed to with 'CtorType'
  | NonEmptyType (TypeDesc v)
  -- ^ nanopass has built-in knowledge of non-empty lists, so they are represented specially as opposed to with 'CtorType'
  | UnitType
  -- ^ nanopass has built-in knowledge of the unit type, so they are represented specially as opposed to with 'CtorType'
  | TupleType (TypeDesc v) (TypeDesc v) [TypeDesc v]
  -- ^ nanopass has built-in knowledge of the tuple types, so they are represented specially as opposed to with 'CtorType'
  deriving(Eq,Show)

---------------------------------
------ Modifying Languages ------
---------------------------------

data LangMod = LangMod
  { baseLang :: UpDotName
  , newLang :: UpName
  , newParams :: [Name 'Unvalidated LowName]
  , nontermsEdit :: [NontermsEdit]
  , originalModProgram :: Maybe String
  }
  deriving(Show)

data NontermsEdit
  = AddNonterm (Nonterm 'Unvalidated)
  | ModNonterm UpName [ProductionsEdit]
  | DelNonterm UpName
  deriving(Show)

data ProductionsEdit
  = AddProd (Production 'Unvalidated)
  | DelProd UpName
  deriving(Show)

--------------------
------ Passes ------
--------------------

data Pass = Pass
  { sourceLang :: Name 'Unvalidated UpDotName
  , targetLang :: Name 'Unvalidated UpDotName
  }
  deriving (Show)
