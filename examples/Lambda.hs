{-# LANGUAGE QuasiQuotes #-}
module Lambda where

import Language.Nanopass (deflang)


[deflang| Lambda
( Expr
  ( Var String )
  ( Lam {x String} {body $Expr} )
  ( App {f $Expr} {a $Expr} )
)
|]

