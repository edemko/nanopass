{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Nanopass.Internal.Validate
  ( validateLanguage
  , validateParams
  , validateNonterm
  , validateProd
  , validateType
  ) where

import Nanopass.Internal.Representation

import Data.Functor ((<&>))
import Data.List (nub, (\\))
import Control.Monad (forM,when)
import Data.Map (Map)
import Data.Set (Set)
import Nanopass.Internal.Parser (Error(..))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Haskell.TH as TH

validateLanguage :: Language 'Unvalidated UpName -> Either Error (Language 'Valid UpName)
validateLanguage lang = do
  langParams <- validateParams lang.langInfo.langParams
  let tvs = Map.fromList $ langParams <&> \n -> (n.name, n)
      nts = Set.fromList $ Map.keys lang.langInfo.nonterms
  nonterms <- validateNonterm nts tvs `mapM` lang.langInfo.nonterms
  pure Language
    { langName = ValidName lang.langName.name (TH.mkName $ fromUpName lang.langName.name)
    , langInfo = LanguageInfo
      { langParams
      , nonterms
      , originalProgram = lang.langInfo.originalProgram
      , baseDefdLang = lang.langInfo.baseDefdLang
      }
    }

validateParams :: [Name 'Unvalidated LowName] -> Either Error [Name 'Valid LowName]
validateParams tvs = do
  let duplicates = tvs \\ nub tvs
  when (not $ null duplicates) $ Left $ DuplicateLanguageParams (duplicates <&> (.name))
  forM tvs $ \n -> pure $ ValidName n.name (TH.mkName $ fromLowName n.name)

validateNonterm :: Set UpName -- ^ known non-terminals
                -> Map LowName (Name 'Valid LowName) -- ^ known type variables
                -> Nonterm v
                -> Either Error (Nonterm 'Valid)
validateNonterm nts tvs nt = do
  let nontermName = ValidName nt.nontermName.name (TH.mkName $ fromUpName nt.nontermName.name)
  productions <- validateProd nts tvs `mapM` nt.productions
  pure Nonterm
    { nontermName
    , productions
    }

validateProd :: Set UpName -- ^ known non-terminals
             -> Map LowName (Name 'Valid LowName) -- ^ known type variables
             -> Production v
             -> Either Error (Production 'Valid)
validateProd nts tvs prod = do
  let prodName = ValidName prod.prodName.name (TH.mkName $ fromUpName prod.prodName.name)
  subterms <- validateType nts tvs `mapM` prod.subterms
  pure Production
    { prodName
    , subterms
    }

validateType :: Set UpName -- ^ known non-terminals
             -> Map LowName (Name 'Valid LowName) -- ^ known type variables
             -> TypeDesc v
             -> Either Error (TypeDesc 'Valid)
validateType nts tvs = \case
  RecursiveType n -> case Set.member n nts of
    True -> pure $ RecursiveType n
    False -> Left $ UnrecognizedNonterm n
  VarType n -> case Map.lookup n.name tvs of
    Just validName -> pure $ VarType validName
    Nothing -> Left $ UnrecognizedTypeVariable n.name
  CtorType n ts
    | ([], n') <- splitUpDotName n.name
    , n' `Set.member` nts
    -> case ts of
      [] -> pure $ RecursiveType n'
      _ -> Left $ UnexpectedTypeApplicationstoRecursiveType n'
  CtorType (SourceName n) ts -> do
    let ctor = ValidName n (TH.mkName $ fromUpDotName n)
    CtorType ctor <$> loop `mapM` ts
  CtorType ctor@(ValidName _ _) ts -> do
    CtorType ctor <$> loop `mapM` ts
  ListType t -> ListType <$> loop t
  MaybeType t -> MaybeType <$> loop t
  NonEmptyType t -> NonEmptyType <$> loop t
  UnitType -> pure UnitType
  TupleType t1 t2 ts -> TupleType <$> loop t1 <*> loop t2 <*> loop `mapM` ts
  where loop = validateType nts tvs
