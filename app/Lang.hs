{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang where

import Language.Nanopass.QQ (deflang)

[deflang| L0 (funny)
  (Expr
    (Var {x String})
    (Lam {x String} {e ($Stmt *)})
    (App {f $Expr} {a $Expr})
    (Nope String)
    (UhOh {$Expr $Expr})
  )
  (Stmt
    (Expr {delme funny} $Expr)
    (Let {x String} {e $Expr})
  )
|]
deriving stock instance (Show funny) => Show (Expr funny)
deriving stock instance (Show funny) => Show (Stmt funny)
