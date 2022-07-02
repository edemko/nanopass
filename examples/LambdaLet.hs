{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module LambdaLet where

import Language.Nanopass
import Lambda qualified as L0


[deflang| L0.Lambda :-> LambdaLet
( *
  ( Expr
    ( + Let { bind ({String $Expr} +) } { letIn $Expr }
    )
  )
)
|]

$(pure [])

[defpass| LambdaLet :-> L0.Lambda |]

compile = descendExpr xlate where
  xlate = Xlate
    { exprLet =
        \ne expr → compile $ foldr unlet expr ne
    , expr =
        const Nothing
    }
    where
      unlet (x, e) body =
        (Lam x body) `App` e

