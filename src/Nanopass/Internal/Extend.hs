{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Nanopass.Internal.Extend
  ( extendLang
  , partitionNontermsEdits
  , EditingNonterms
  , addNonterms
  , modNonterms
  , delNonterms
  , extendProductions
  , partitionProductionsEdits
  ) where

import Prelude hiding (mod)
import Nanopass.Internal.Representation

import Control.Monad (forM_,when)
import Data.Functor ((<&>))
import Data.List (nub, (\\))
import Data.Map (Map)
import Data.Set (Set)
import Nanopass.Internal.Error (Error(..))
import Nanopass.Internal.Validate (validateParams,validateNonterm,validateProd)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Haskell.TH as TH

extendLang :: Language 'Valid UpDotName -> LangMod -> Either Error (Language 'Valid UpName)
extendLang orig mod = do
  let (additions, mods, deletions) = partitionNontermsEdits mod.nontermsEdit
  forM_ mods $ \(n, _) -> case (n `elem` ((.nontermName.name) <$> additions), n `elem` deletions) of
    (_, True) -> Left $ IllegalNontermModificationAlsoDeleted n
    (True, _) -> Left $ IllegalNontermModificationAlsoAdded n
    _ -> pure ()
  forM_ additions $ \add -> case add.nontermName.name `elem` deletions of
    True -> Left $ IllegalNontermAddedAlsoDeleted add.nontermName.name
    _ -> pure ()
  do
    let modNames = fst <$> mods
        duplicates = modNames \\ nub modNames
    when (not $ null duplicates) $
      Left $ DuplicateNontermMods modNames
  let modifications = Map.fromList mods
  params <- validateParams mod.newParams
  let tvs = Map.fromList $ params <&> \n -> (n.name, n)
      nts = (Set.fromList (Map.keys orig.langInfo.nonterms)
              `Set.difference` Set.fromList deletions)
            `Set.union` Set.fromList ((.nontermName.name) <$> additions)
  newNonterms <-  pure (orig.langInfo.nonterms)
              >>= delNonterms deletions
              >>= modNonterms nts tvs modifications
              >>= addNonterms nts tvs additions
  pure Language
    { langName = ValidName mod.newLang (TH.mkName $ fromUpName mod.newLang)
    , langInfo = LanguageInfo
      { langParams = params
      , nonterms = newNonterms
      , originalProgram = mod.originalModProgram
      , baseDefdLang = Just orig
      }
    }

----------------------------------
------ Nonterminals Editing ------
----------------------------------

partitionNontermsEdits :: [NontermsEdit] -> ([Nonterm 'Unvalidated], [(UpName, [ProductionsEdit])], [UpName])
partitionNontermsEdits = loop ([], [], [])
  where
  loop (as, ms, ds) [] = (reverse as, reverse ms, reverse ds)
  loop (as, ms, ds) (x:xs) = case x of
    AddNonterm a -> loop (a:as, ms, ds) xs
    ModNonterm n m -> loop (as, (n, m):ms, ds) xs
    DelNonterm d -> loop (as, ms, d:ds) xs

type EditingNonterms = Map UpName (Nonterm 'Valid)

addNonterms :: Set UpName -- ^ known non-terminals for the new language
            -> Map LowName (Name 'Valid LowName) -- ^ known type variables for the new language
            -> [Nonterm 'Unvalidated] -- ^ new non-terminals to add
            -> EditingNonterms -- ^ old language's non-terminals
            -> Either Error EditingNonterms
addNonterms nts tvs adds orig = loop orig adds
  where
  loop !new [] = pure new
  loop !new (add:rest) = do
    case Map.lookup add.nontermName.name new of
      Just _ -> Left $ IllegalNontermAdded add.nontermName.name
      Nothing -> pure ()
    okAdd <- validateNonterm nts tvs add
    loop (Map.insert okAdd.nontermName.name okAdd new) rest

modNonterms :: Set UpName -- ^ known non-terminals for the new language
            -> Map LowName (Name 'Valid LowName) -- ^ known type variables for the new language
            -> Map UpName [ProductionsEdit] -- ^ edits to various non-terminals' productions
            -> EditingNonterms -- ^ old language's non-terminals
            -> Either Error EditingNonterms
modNonterms nts tvs mods orig = do
  forM_ (Map.keys mods) $ \n -> case Map.lookup n orig of
    Nothing -> Left $ IllegalNontermModified n
    Just _ -> pure ()
  forWithKeyM orig $ \n oldNonterm -> case Map.lookup n mods of
    Just prodsEdit -> do
      let newName = ValidName oldNonterm.nontermName.name (TH.mkName $ fromUpName oldNonterm.nontermName.name)
      newProductions <- extendProductions nts tvs oldNonterm.productions prodsEdit
      pure $ Nonterm
        { nontermName = newName
        , productions = newProductions
        }
    Nothing -> validateNonterm nts tvs oldNonterm

delNonterms :: [UpName] -- ^ names of non-terminals to remove
            -> EditingNonterms -- ^ old language's non-terminals
            -> Either Error EditingNonterms
delNonterms dels orig = loop orig dels
  where
  loop !new [] = pure new
  loop !new (del:rest) = case Map.lookup del new of
    Just _ -> loop (Map.delete del new) rest
    Nothing -> Left $ IllegalNontermDeleted del

--------------------------------
------ Production Editing ------
--------------------------------

extendProductions :: Set UpName -- ^ known non-terminals for the new language
                  -> Map LowName (Name 'Valid LowName) -- ^ known type variables for the new language
                  -> (Map UpName (Production 'Valid))
                  -> [ProductionsEdit]
                  -> Either Error (Map UpName (Production 'Valid))
extendProductions nts tvs orig mods = do
  let (additions, deletions) = partitionProductionsEdits mods
  -- NOTE this forM_ is over-restricive because I don't have a way to modify/outright replace productions
  -- forM_ additions $ \add -> case add.prodName.name `elem` deletions of
  --   True -> Left $ IllegalProductionAddedAlsoDeleted add.prodName.name
  --   _ -> pure ()
  restricted <- delProductions deletions orig
  revalidated <- forWithKeyM restricted $ \_ oldProd ->
    validateProd nts tvs oldProd
  addProductions nts tvs additions revalidated

partitionProductionsEdits :: [ProductionsEdit] -> ([Production 'Unvalidated], [UpName])
partitionProductionsEdits = loop ([], [])
  where
  loop (as, ds) [] = (reverse as, reverse ds)
  loop (as, ds) (x:xs) = case x of
    AddProd a -> loop (a:as, ds) xs
    DelProd d -> loop (as, d:ds) xs

addProductions :: Set UpName -- ^ known non-terminals for the new language
               -> Map LowName (Name 'Valid LowName) -- ^ known type variables for the new language
               -> [Production 'Unvalidated]
               -> (Map UpName (Production 'Valid))
               -> Either Error (Map UpName (Production 'Valid))
addProductions nts tvs adds orig = loop orig adds
  where
  loop !new [] = pure new
  loop !new (add:rest) = case Map.lookup add.prodName.name new of
    Just _ -> Left $ IllegalProductionAdded add.prodName.name
    Nothing -> do
      okAdd <- validateProd nts tvs add
      loop (Map.insert okAdd.prodName.name okAdd new) rest

delProductions :: [UpName]
               -> Map UpName (Production 'Valid)
               -> Either Error (Map UpName (Production 'Valid))
delProductions dels orig = loop orig dels
  where
  loop !new [] = pure new
  loop !new (del:rest) = case Map.lookup del new of
    Just _ -> loop (Map.delete del new) rest
    Nothing -> Left $ IllegalProductionDeleted del

---------------------
------ Helpers ------
---------------------

forWithKeyM :: (Ord k, Applicative f) => Map k a -> (k -> a -> f b) -> f (Map k b)
forWithKeyM m f = sequenceA $ Map.mapWithKey f m
