{-# LANGUAGE DuplicateRecordFields #-}

-- | This module holds type definitions that describe the internal
-- representation of language syntaxen as understood by nanopass.
module Nanopass.Internal.Representation
  (
  -- * Types for Base Languages
  -- $ ir
    Language(..)
  , Syncat(..)
  , Production(..)
  , TypeDesc(..)
  -- * Types for Modifying Manguages
  , LangMod(..)
  , SyncatMod(..)
  , ProdMod(..)
  -- * Helper Types
  , UpName, toUpName, fromUpName
  , LowName, toLowName, fromLowName
  , UpDotName, toUpDotName, fromUpDotName
  , unDotted, upDotQualifier, upDotBase, upDotChBase
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

data UpDotName = UpDotName [UpName] UpName
  deriving (Show)

-- | Introduction form for 'UpDotName'
toUpDotName :: String -> Maybe UpDotName
toUpDotName = loop []
  where
  loop acc inp = case break (== '.') inp of
    ([], _) -> Nothing -- no leading dot, double dot, or empty string allowed
    (_, ".") -> Nothing -- no trailing dot allowed
    (str, []) -> do -- no more dots
      base <- toUpName str
      pure $ UpDotName (reverse acc) base
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


isAlphaNumderscore :: Char -> Bool
isAlphaNumderscore c = isAlphaNum c || c == '_'

----------------------------
------ Base Languages ------
----------------------------

-- $ir
--
-- The types 'Language', 'Syncat', 'Production' mediate between Haskell and the theory of context-free grammars (CFGs).
-- Each of them is an intermediate representation that can be seen from two perspectives:
--
-- * What Haskell concept do they map to?
-- * What CFG concept do they map to?
--
-- We use something like usual, minimal definition of a CFG as a 4-tuple G = (V, Σ, R, S) where
--
-- 1. V is a set of non-terminals (named by 'syncatName')
-- 2. Σ is a set of terminals (which are just ordinary Haskell data types)
-- 3. R is a relation in V × (V ∪ Σ)*. Members of this relation are called rewrite rules (and map to the arguments of a Haskell data constructor).
-- 4. S is the start symbol, though it is not used by nanopass.

data Language = Language
  { langName :: !UpDotName
  , langNameTH :: !TH.Name
  , langParams :: ![LowName]
  , syncats :: !(Map UpName Syncat)
  , originalProgram :: !(Maybe String)
  , baseDefdLang :: !(Maybe Language)
  }
  deriving(Show)

data Syncat = Syncat
  { syncatName :: !UpName
  , syncatNameTH :: !TH.Name
  , productions :: !(Map UpName Production)
  }
  deriving(Show)

-- | Seen as a Haskell entity, each 'Production' maps to a constructor for a data type.
-- Seen from the perspective of a CFG, each 'Production' maps to a single rewrite rule.
data Production = Production
  { prodName :: !UpName
  , prodNameTH :: !TH.Name
  , subterms :: ![TypeDesc]
  }
  deriving(Show)

data TypeDesc
  = RecursiveType UpName -- these are metavariables that start with a lowercase letter
  | VarType TH.Name -- TODO should this really be a `TH.Name`?
  | CtorType TH.Name [TypeDesc] -- the string here will be used to look up a type in scope at the splice site, and will start with an uppercase letter
  | ListType TypeDesc -- because otherwise, you'd have to always be saying `type List a = [a]`
  | MaybeType TypeDesc
  | NonEmptyType TypeDesc
  | TupleType TypeDesc TypeDesc [TypeDesc]
  | MapType TypeDesc TypeDesc
  deriving(Eq,Show)

---------------------------------
------ Modifying Languages ------
---------------------------------

data LangMod = LangMod
  { baseLang :: UpDotName
  , newLang :: UpName
  , newParams :: [LowName]
  , syncatMods :: [SyncatMod]
  , originalModProgram :: Maybe String
  }
  deriving(Show)

data SyncatMod
  = AddSyncat Syncat
  | ModProds UpName [ProdMod]
  | DelSyncat UpName
  deriving(Show)

data ProdMod
  = AddProd Production
  | DelProd UpName
  deriving(Show)
