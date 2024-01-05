{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module holds type definitions that describe the internal
-- representation of language syntaxen as understood by nanopass.
module Nanopass.Internal.Representation
  (
  -- * Types for Base Languages
  -- $ ir
    Language(..)
  , Nonterm(..)
  , Production(..)
  , TypeDesc(..)
  -- * Types for Modifying Manguages
  , LangMod(..)
  , NontermsEdit(..)
  , ProductionsEdit(..)
  -- * Helper Types
  , UpName, toUpName, fromUpName
  , LowName, toLowName, fromLowName
  , UpDotName, toUpDotName, fromUpDotName
  , unDotted, upDotQualifier, upDotBase, upDotChBase
  , Name(..), Validate(..)
  ) where

import Data.Char (isLower,isUpper,isAlphaNum)
import Data.List (intercalate)
import Data.Map (Map)

import qualified Language.Haskell.TH as TH

------ Names ------

-- | Strings matching @[A-Z][a-zA-Z0-9_]@
newtype UpName = UpName String
  deriving (Show,Eq,Ord)

-- | Introduction form for 'UpName'
toUpName :: String -> Maybe UpName
toUpName (c:cs) | isUpper c && all isAlphaNumderscore cs = Just $ UpName (c:cs)
toUpName _ = Nothing

-- | Elimination form for 'UpName'
fromUpName :: UpName -> String
fromUpName (UpName str) = str

-- | Strings matching @[a-z][a-zA-Z0-9_]@
newtype LowName = LowName String
  deriving (Show,Eq,Ord)

-- | Introduction form for 'LowName'
toLowName :: String -> Maybe LowName
toLowName (c:cs) | isLower c && all isAlphaNumderscore cs = Just $ LowName (c:cs)
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


data Validate = Valid | Unvalidated
data Name v n where
  SourceName :: { name :: n } -> Name 'Unvalidated n
  ValidName :: { base :: n, th :: TH.Name } -> Name 'Valid n
deriving instance (Show n) => Show (Name v n)
deriving instance (Eq n) => Eq (Name v n)
deriving instance (Ord n) => Ord (Name v n)


isAlphaNumderscore :: Char -> Bool
isAlphaNumderscore c = isAlphaNum c || c == '_'

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

data Language v = Language
  { langName :: !(Name v UpDotName)
  , langParams :: ![Name v LowName]
  , nonterms :: !(Map UpName (Nonterm v))
  , originalProgram :: !(Maybe String)
  , baseDefdLang :: !(Maybe (Language 'Valid))
  }
  deriving(Show)

data Nonterm v = Nonterm
  { nontermName :: !(Name v UpName)
  , productions :: !(Map UpName (Production v))
  }
  deriving(Show)

-- | Seen as a Haskell entity, each 'Production' maps to a constructor for a data type.
-- Seen from the perspective of a CFG, each 'Production' maps to a single rewrite rule.
data Production v = Production
  { prodName :: !(Name v UpName)
  , subterms :: ![TypeDesc v]
  }
  deriving(Show)

data TypeDesc v
  = RecursiveType UpName
  | VarType (Name v LowName)
  | CtorType (Name v UpDotName) [TypeDesc v]
  | ListType (TypeDesc v) -- because otherwise, you'd have to always be saying `type List a = [a]`
  | MaybeType (TypeDesc v)
  | NonEmptyType (TypeDesc v)
  | UnitType
  | TupleType (TypeDesc v) (TypeDesc v) [TypeDesc v]
  deriving(Eq,Show)

---------------------------------
------ Modifying Languages ------
---------------------------------

data LangMod = LangMod
  { baseLang :: UpDotName
  , newLang :: UpName
  , newParams :: [LowName]
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
