-- | This module holds type definitions that describe the internal
-- representation of language syntaxen as understood by nanopass.
module Nanopass.Internal.LangDef
  ( UpName, toUpName, fromUpName
  , LowName, toLowName, fromLowName
  , UpDotName, toUpDotName, fromUpDotName
  , upDotQualifier, upDotBase, upDotChBase

  , DefdLang(..)
  , DefdSyncatType(..)
  , DefdProd(..)
  , TypeDesc(..)
  , LangDef(..)
  , SyncatDef(..)
  , ProdDef(..)
  , LangMod(..)
  , SyncatMod(..)
  , ProdMod(..)
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

upDotQualifier :: UpDotName -> [UpName]
upDotQualifier (UpDotName xs _) = xs

upDotBase :: UpDotName -> UpName
upDotBase (UpDotName _ x) = x

upDotChBase :: UpDotName -> UpName -> UpDotName
upDotChBase (UpDotName xs _) y = UpDotName xs y

isAlphaNumderscore :: Char -> Bool
isAlphaNumderscore c = isAlphaNum c || c == '_'

------ Old Stuff ------

data LangDef = LangDef
  { langNameReq :: UpName
  , langParamReqs :: [LowName]
  , syncatReqs :: [SyncatDef]
  , originalProgram :: Maybe String
  , baseDefdLang :: Maybe DefdLang
  }
  deriving(Show)

data SyncatDef = SyncatDef
  { syncatNameReq :: UpName
  , productionReqs :: [ProdDef]
  }
  deriving(Show)

data ProdDef = ProdDef
  { prodNameReq :: UpName
  , subtermReqs :: [TypeDesc]
  }
  deriving(Show)



data LangMod = LangMod
  { baseLangReq :: UpDotName
  , newLangReq :: UpName
  , newParamReqs :: [LowName]
  , syncatMods :: [SyncatMod]
  , originalModProgram :: Maybe String
  }
  deriving(Show)

data SyncatMod
  = AddSyncat SyncatDef
  | DelSyncat UpName
  | ModProds
    { syncatName :: UpName
    , prodMods :: [ProdMod]
    }
  deriving(Show)

data ProdMod
  = AddProd ProdDef
  | DelProd UpName
  deriving(Show)







data DefdLang = DefdLang
  { defdLangName :: UpDotName
  , defdLangNameTH :: TH.Name
  , defdLangParams :: [TH.Name]
  , defdSyncats :: Map UpName DefdSyncatType
  }
  deriving(Show)

data DefdSyncatType = DefdSyncatType
  { defdSyncatName :: UpName
  , defdSyncatNameTH :: TH.Name
  , defdProds :: Map UpName DefdProd
  }
  deriving(Show)

data DefdProd = DefdProd
  { defdProdName :: UpName
  , defdProdNameTH :: TH.Name
  , defdSubterms :: [TypeDesc]
  }
  deriving(Show)

data TypeDesc
  = RecursiveType UpName -- these are metavariables that start with a lowercase letter
  | VarType TH.Name
  | CtorType TH.Name [TypeDesc] -- the string here will be used to look up a type in scope at the splice site, and will start with an uppercase letter
  | ListType TypeDesc -- because otherwise, you'd have to always be saying `type List a = [a]`
  | MaybeType TypeDesc
  | NonEmptyType TypeDesc
  | TupleType TypeDesc TypeDesc [TypeDesc]
  | MapType TypeDesc TypeDesc
  deriving(Eq,Show)
