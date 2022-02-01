TODO

## 0.0.2.0

* a pure interface for translation
  ```
  data XlateId funny = XlateId
    { exprLam :: String -> [L0.Stmt funny] -> L1.Expr
    , expr :: L0.Expr funny -> Maybe L1.Expr
    }
  idXlate :: XlateId funny -> Xlate funny f
  idXlate xlate = Xlate
    { exprLam = \x e -> Identity $ xlate.exprLam x e
    , expr = \l0 -> Identity <$> xlate.expr l0
    }

  descendExprId :: XlateId funny -> L0.Expr funny -> L1.Expr
  descendExprId xlate = runIdentity . exprDescend (idXlate xlate)
  ```

* generate documentation
  * the members of Xlate


* move the XlateDef's constructors into the xlateSyncats?


## 0.0.3.0

* allow the user to give an allow/deny list for syncat overrides, rather than generating all overrides by default

* look into [multiplate](https://wiki.haskell.org/Multiplate) for within-language traversals
  (one of the few times the HaskellWiki supplies the best documentation)

* examples/
  * common/
  * norm-parens (remove all any only unnecessary parentheses from lambda terms)
  * more

* allow user comments on productions and subterms

As features start to become unmanageable, I may want to split nanopass into nanopass-boot, and a true nanopass, which is built with nanopass (i.e. nanopass-boot)