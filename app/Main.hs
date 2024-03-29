{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Nanopass (deflang,defpass)
import Text.Pretty.Simple (pPrint)

import qualified Lang as L0

[deflang|
(L1 from L0:L0
  (* Expr
    (- Lam)
    (+ Lam String Expr)
    (- Nope)
  )
  (- Stmt)
)
|]
deriving stock instance Show Expr

$(pure [])

[defpass|(from L0:L0 to L1)|]

main :: IO ()
main = do
  let theF = L0.Lam "x"
            [ L0.Let "y" $ L0.Var "x"
            , L0.Expr () $ L0.Var "y"
            ]
      theE = L0.App theF (L0.Var "foo")
  pPrint theE
  pPrint $ compile theE

compile :: L0.Expr () -> Expr
compile = descendExprI xlate
  where
  xlate = XlateI
    { onExprI = const Nothing
    , onExprLamI = \var body -> case body of
        [] -> Lam var $ Var var
        L0.Expr () e1 : _ -> Lam var $ compile e1
        L0.Let _ body1 : _ -> Lam var $ compile body1
    , onExprNopeI = Var
    }
