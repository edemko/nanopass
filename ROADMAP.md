TODO

## 0.0.2.0

* a pure interface for translation
  ```
  data Xlate funny = Xlate
    { exprLam :: String -> [L0.Stmt funny] -> L1.Expr
    , expr :: L0.Expr funny -> Maybe L1.Expr
    }
  idXlate :: Xlate funny -> XlateA funny f
  idXlate xlate = XlateA
    { exprLam = \x e -> Identity $ xlate.exprLam x e
    , expr = \l0 -> Identity <$> xlate.expr l0
    }

  descendExpr :: Xlate funny -> L0.Expr funny -> L1.Expr
  descendExpr xlate = runidentity . exprDescendA (idXlate xlate)
  ```

* move the XlateDef's constructors into the xlateSyncats

* use getDoc/putDoc to write an explanation of the generated languages/translators

## 0.0.3.0

* allow the user to give an allow/deny list for syncat overrides, rather than generating all overrides by default

* look into [multiplate](https://wiki.haskell.org/Multiplate) for within-language traversals
  (one of the few times the HaskellWiki supplies the best documentation)

* examples/
  * common/
  * norm-parens (remove all any only unnecessary parentheses from lambda terms)
  * more

