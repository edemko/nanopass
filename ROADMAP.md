## 0.0.3.0

* allow to prefix the names of generated named from `defpass`

* allow the user to give an allow/deny list for syncat overrides, rather than generating all overrides by default

* look into [multiplate](https://wiki.haskell.org/Multiplate) for within-language traversals
  (one of the few times the HaskellWiki supplies the best documentation)

* examples/
  * common/
  * norm-parens (remove all and only unnecessary parentheses from lambda terms)
  * more

* allow user comments on productions and subterms

As features start to become unmanageable, I may want to split nanopass into nanopass-boot, and a true nanopass, which is built with nanopass (i.e. nanopass-boot)