{-# LANGUAGE LambdaCase #-}

module Language.Nanopass.LangDef where

import Control.Monad (forM,forM_)
import Control.Monad.State (StateT,gets,modify,evalStateT)
import Data.Functor ((<&>))
import Data.List (nub,(\\))
import Data.Map (Map)
import Language.Haskell.TH (Q, Dec)

import qualified Control.Monad.Trans as M
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH


data LangDef = LangDef [(String, [CtorDef])]
data CtorDef = CtorDef String [(Maybe String, TypeDesc)]
data TypeDesc
  = GrammarType String -- these are metavariables that start with a lowercase letter
  | CtorType String [TypeDesc] -- the string here will be used to look up a type in scope at the splice site, and will start with an uppercase letter
  | ListType TypeDesc -- because otherwise, you'd have to always be saying `type List a = [a]`
  -- TODO tuple types

type Define a = StateT DefState Q a
data DefState = DefState
  { annotationType :: TH.Name
  , nonterminalNames :: Map String TH.Name
  }

run :: Define a -> Q a
run = flip evalStateT (DefState (error "use annotation type before init") Map.empty)


defineLang :: LangDef -> Define [Dec]
defineLang (LangDef grammars) = do
  -- initialize nonterminalNames
  forM_ (fst <$> grammars) $ \lName -> do
    knownNames <- gets nonterminalNames
    case Map.lookup lName knownNames of
      Nothing -> modify $ \st ->
        st{nonterminalNames = Map.insert lName (TH.mkName lName) knownNames}
      Just _ -> fail $ concat [ "in a nanopass language definition: "
                              , "duplicate nonterminal name "
                              , lName
                              ]
  forM grammars $ \(lName, ctorRequests) -> do
    -- create a new type variable for annotations
    annTy <- M.lift (TH.newName "ann")
    modify $ \st -> st{annotationType = annTy}
    ctors <- defineCtor lName `mapM` ctorRequests
    pure $ TH.DataD [] (TH.mkName lName) [TH.PlainTV annTy] Nothing
            ctors
            [] -- TODO deriving clauses

defineCtor :: String -- name of the nonterminal grammar type
            -> CtorDef
            -> Define TH.Con
defineCtor grammarName (CtorDef termName memberRequests) = do
  members <- sequence $ zip [(1 :: Int)..] memberRequests <&> \case
    (_, (Just explicitName, v))
      | explicitName == "annotation" -> fail $ concat [ "in a nanopass language definition: "
                                                      , "a term member cannot be named 'annotation'"
                                                      ]
      | otherwise -> pure (explicitName, v)
    (i, (Nothing, v)) ->
      let gensym = "_" ++ grammarName ++ termName ++ "_" ++ show i
       in pure (gensym, v)
  let duplicateNames = (fst <$> members) \\ nub (fst <$> members)
  annField <- do
    annTy <- gets annotationType
    pure (TH.mkName "annotation", noBang, TH.VarT annTy)
  fields <- case duplicateNames of
    [] -> mapM defineMember members
    _ -> fail $ concat [ "in a nanopass language definition: "
                       , "the following members were defined more than once in a term constructor"
                       , show (nub duplicateNames)
                       ]
  pure $ TH.RecC (TH.mkName termName) (annField : fields)

defineMember :: (String, TypeDesc) -> Define TH.VarBangType
defineMember (lName, typeDesc) = do
  ty <- defineArg typeDesc
  let bang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
  pure (TH.mkName lName, noBang, ty)

defineArg :: TypeDesc -> Define TH.Type
defineArg (GrammarType lName) =
  gets (Map.lookup lName . nonterminalNames) >>= \case
    Just thName -> do
      let grammarCtor = TH.ConT thName
      ann <- TH.VarT <$> gets annotationType
      pure $ TH.AppT grammarCtor ann
    Nothing -> fail $ concat ["in a nanopass language definition: unknown metavariable ", lName]
defineArg (CtorType hsName argDescs) = do
  thName <- M.lift (TH.lookupTypeName hsName) >>= \case
    Just thName -> pure $ TH.ConT thName
    Nothing -> fail $ concat ["in a nanopass language definition: type name not in scope ", hsName]
  args <- defineArg `mapM` argDescs
  pure $ foldl TH.AppT thName args
defineArg (ListType argDesc) = do
  arg <- defineArg argDesc
  pure $ TH.AppT TH.ListT arg

noBang :: TH.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
