# Revision history for nanopass

## 0.0.3.0 -- YYYY-mm-dd


## 0.0.2.1 -- 2024-01-02

* Fix bug decoding constructors with no fields
* Add missing "modifier" production rules to documentation
* Allow parentheses drop when a syncat modifier has exactly one child
* add `on` prefix to Xlate records to avoid ambiguous records
* require `Xlate{,I}` fields to be strict so they don't get forgotten when you write translators
* because nanopass creates partial-fields (detectable with `-Wpartial-fields`),
  I've decided to prefix these with double-underscore to indicate that they are not meant for use
  beyond nanopass' internals

## 0.0.2.0 -- 2022-02-11

* Generate documentation for the members of `Xlate` and `XlateI`.
* Add generation of pure variants of translation facilities to `defpass`.
* Change `{Xlate,descend*}A` names to drop the `A`; applicative is probably the more common case.
* Fix bug in testing for `Traversable` instance.
* Generate documentation for generated types/functions.
* Requires template-haskell >=2.18, and therefore GHC 9.2.1

## 0.0.1.0 -- 2022-01-26

* First version. Unreleased in any unsuspecting world.
