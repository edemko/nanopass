module Lang where

import Language.Haskell.TH (mkName)
import Language.Nanopass.LangDef

lang :: LangDef
lang = LangDef "L0"
  [ GrammarDef "Expr"
    [ CtorDef "Var"
      [ SubtermDef (Just "x") $ CtorType (mkName "String") []
      ]
    , CtorDef "Lam"
      [ SubtermDef (Just "x") $ CtorType (mkName "String") []
      , SubtermDef (Just "e") $ ListType (GrammarType "Stmt")
      ]
    , CtorDef "App"
      [ SubtermDef (Just "f") $ GrammarType "Expr"
      , SubtermDef (Just "a") $ GrammarType "Expr"
      ]
    ]
  , GrammarDef "Stmt"
    [ CtorDef "Expr"
      [ SubtermDef Nothing $ GrammarType "Expr"
      ]
    , CtorDef "Let"
      [ SubtermDef (Just "x") $ CtorType (mkName "String") []
      , SubtermDef (Just "e") $ GrammarType "Expr"
      ]
    ]
  ]