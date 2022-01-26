{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.Pretty.Simple (pPrint)

import Language.Nanopass.Xlate
import Data.Functor.Identity
import Language.Nanopass.LangDef

import qualified Lang as L0
import qualified Lang1 as L1

$(do { l1 <- reifyLang "L0.L0" ; l2 <- reifyLang "L1.L1" ; mkXlateA l1 l2 })

main :: IO ()
main = do
  let theF = L0.Lam "x"
            [ L0.Let "y" $ L0.Var "x"
            , L0.Expr () $ L0.Var "y"
            ]
      e = L0.App theF (L0.Var "foo")
  pPrint e
  pPrint $ compile e

compile :: L0.Expr () -> L1.Expr
compile = runIdentity . descendExprA xlate
  where
  xlate = XlateA
    { expr = const Nothing
    , exprLam = \x body -> pure $ case body of
        [] -> L1.Lam x $ L1.Var x
        L0.Expr () e : _ -> L1.Lam x $ compile e
        L0.Let _ e : _ -> L1.Lam x $ compile e
    , exprNope = \x -> pure $ L1.Var x
    }
