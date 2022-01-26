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
      (+ Lam {x String} {e $Expr})
      (- Nope)
    ))
  (- Stmt)
|]
deriving stock instance Show Expr
