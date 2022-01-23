{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang1 where

import Language.Nanopass.QQ (deflang)

import qualified Lang as L0

[deflang| L0.L0 :-> L1
  (*
    (Expr
      (- Lam)
      (+ Lam {x String} {e expr})
    ))
  (- Stmt)
|]
deriving stock instance (Show a) => Show (Expr a)

-- ext :: LangMod
-- ext = LangMod "L0.L0" "L1"
--   [ ModCtors "Expr"
--     [ DelCtor "Lam"
--     , AddCtor $ CtorDef "Lam"
--       [ SubtermDef (Just "x") $ CtorType (mkName "String") []
--       , SubtermDef (Just "e") $ GrammarType "Expr"
--       ]
--     ]
--   , DelGrammar "Stmt"
--   ]