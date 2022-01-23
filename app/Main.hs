module Main where

import Text.Pretty.Simple (pPrint)

import qualified Lang as L0
import qualified Lang1 as L1


main :: IO ()
main = do
  let theF = L0.Lam () "x"
            [ L0.Let () "y" $ L0.Var () "x"
            , L0.Expr () $ L0.Var () "y"
            ]
      expr = L0.App () theF (L0.Var () "foo")
  pPrint expr
  pPrint $ rewrite expr


rewrite :: L0.Expr a -> L1.Expr a
rewrite (L0.Lam ann x []) = L1.Lam ann x $ L1.Var ann x
rewrite (L0.Lam ann x (L0.Expr _ e : _)) = L1.Lam ann x $ rewrite e
rewrite (L0.Lam ann x (L0.Let _ _ e : _)) = L1.Lam ann x $ rewrite e
rewrite (L0.Var ann x) = L1.Var ann x
rewrite (L0.App ann f a) = L1.App ann (rewrite f) (rewrite a)
