{-# LANGUAGE TemplateHaskell #-}
module Lang1 where

import Language.Haskell.TH (mkName)
import Language.Nanopass.LangDef

-- import qualified Lang.TH as L0

ext :: LangMod
ext = LangMod "L0.L0" "L1"
  [ ModCtors "Expr"
    [ DelCtor "Lam"
    , AddCtor $ CtorDef "Lam"
      [ SubtermDef (Just "x") $ CtorType (mkName "String") []
      , SubtermDef (Just "e") $ GrammarType "Expr"
      ]
    ]
  , DelGrammar "Stmt"
  ]