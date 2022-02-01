-- | Nanopass consists essentially of creating languages and defining passes.
-- Languages can be created from scratch or by derivation using 'deflang'.
-- The tedious parts of a compiler pass (or at least, most passes) can be generated with 'defpass'.
--
-- More details and examples are given in the [readme](https://github.com/edemko/nanopass/blob/master/README.md).
module Language.Nanopass
  ( deflang
  , defpass
  ) where

import Language.Nanopass.QQ (deflang,defpass)