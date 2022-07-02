{-# LANGUAGE OverloadedLists #-}
module Play where

import Lambda qualified as L0
import LambdaLet


e1 =
  Let
    [ "id" := Lam "a" --> Var "a"
    ]

  `In` Var "id"

pattern In   a   = a
pattern (:=) a b = (a,b) ; (-->) = id
infixr 0 :=, -->

----

deriving instance Show Expr
deriving instance Show L0.Expr

foo = compile e1
