{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lang
import Language.Nanopass.LangDef

import Text.Pretty.Simple (pPrint)

$(run $ defineLang lang)

deriving stock instance (Show a) => Show (Expr a)
deriving stock instance (Show a) => Show (Stmt a)

main :: IO ()
main = do
  let theF = Lam () "x"
            [ Let () "y" $ Var () "x"
            , Expr () $ Var () "y"
            ]
  pPrint $ App () theF (Var () "foo")
