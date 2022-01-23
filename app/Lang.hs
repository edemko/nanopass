{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang where

import Language.Nanopass.QQ (deflang)

[deflang| L0
  (Expr
    (Var {x String})
    (Lam {x String} {e (stmt *)})
    (App {f expr} {a expr})
  )
  (Stmt
    (Expr expr)
    (Let {x String} {e expr})
  )
|]
deriving stock instance (Show a) => Show (Expr a)
deriving stock instance (Show a) => Show (Stmt a)
