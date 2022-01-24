module Main where

import Text.Pretty.Simple (pPrint)

import qualified Lang as L0
import qualified Lang1 as L1


main :: IO ()
main = do
  let theF = L0.Lam "x"
            [ L0.Let "y" $ L0.Var "x"
            , L0.Expr () $ L0.Var "y"
            ]
      expr = L0.App theF (L0.Var "foo")
  pPrint expr
  pPrint $ rewrite expr


rewrite :: L0.Expr () -> L1.Expr
rewrite (L0.Lam x []) = L1.Lam x $ L1.Var x
rewrite (L0.Lam x (L0.Expr () e : _)) = L1.Lam x $ rewrite e
rewrite (L0.Lam x (L0.Let _ e : _)) = L1.Lam x $ rewrite e
rewrite (L0.Var x) = L1.Var x
rewrite (L0.App f a) = L1.App (rewrite f) (rewrite a)
