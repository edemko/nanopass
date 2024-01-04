{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Language.Nanopass.QQ
  ( deflang
  , defpass
  ) where


import Language.Nanopass.LangDef
import Nanopass.Internal.Representation
import Prelude hiding (mod)


import Data.Functor ((<&>))
import Language.Haskell.TH (Q, Dec)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Nanopass.Xlate (mkXlate)
import Text.Parse.Stupid (Sexpr(..))

import qualified Data.Text.Lazy as LT
import qualified Language.Haskell.TH as TH
import qualified Nanopass.Internal.Parser as Parser
import qualified Text.Parse.Stupid as Stupid
import qualified Text.Pretty.Simple as PP

-- | Define a language, either from scratch or by derivation from an existing language.
-- The syntax is based on s-expressions.
--
-- TODO document the syntax, or for now you can look in "Nanopass.Internal.Parser"
-- More details and examples are given in the [readme](https://github.com/edemko/nanopass/blob/master/README.md).
deflang :: QuasiQuoter
deflang = QuasiQuoter (bad "expression") (bad "pattern") (bad "type") go
  where
  go :: String -> Q [Dec]
  go input = do
    loc <- TH.location <&> \l -> Parser.Loc
        { Parser.file = l.loc_filename
        , Parser.line = fst l.loc_start
        , Parser.col = snd l.loc_start
        }
    case Parser.parseLanguage (loc, input) of
      (Right (Left def)) -> runDefine $ defineLang def
      (Right (Right mod)) -> runModify mod
      Left err -> fail $ (LT.unpack . PP.pShow) err -- TODO
  bad ctx _ = fail $ "`deflang` quasiquoter cannot be used in " ++ ctx ++ " context,\n\
                     \it can only appear as part of declarations."

-- | Define automatic translation between two langauges.
-- This creates an @Xlate@ type and the @descend\<Syntactic Category\>@ family of functions,
--   as well as pure variants (@XlateI@ and @descend\<Syntactic Category\>I@) and a lifting function @idXlate@.
-- A translation function is generated for each syntactic category with the same name in both source and target languages.
-- At the moment, there is no provision for altering the name of the type or translation function(s),
--   but I expect you'll only want to define one translation per module.
--
-- The @Xlate@ type takes all the parameters from both languages (de-duplicating parameters of the same name),
--   as well as an additional type parameter, which is the functor @f@ under which the translation occurs.
--
-- The type of a @descend\<Syntactic Category\>@ function is
--   @Xlate f → σ → f σ'@.
--
-- If a production in the source language has subterms @τ₁ … τₙ@ and is part of the syntactic category @σ@,
--   then a hole member is a function of type @τ₁ → … τₙ → f σ'@, where @σ'@ is the corresponding syntactic category in the target language.
-- Essentially, you get access all the subterms, and can use the 'Applicative' to generate a target term as long as you don't cross syntactic categories.
--
-- If a source language has syntactic category @σ@ with the same name as the target's syntactic category @σ'@,
--   then an override member is a function of type @σ → 'Maybe' (f σ')@.
-- If an override returns 'Nothing', then the automatic translation will be used,
--   otherwise the automatic translation is ignored in favor of the result under the 'Just'.
--
-- The pure variants have the same form as the 'Applicative' ones, but:
--
--   * @XlateI@ is not parameterized by @f@, nor are the types of its members,
--   * the members of @XlateI@ are suffixed with the letter @I@, and
--   * the types of the @descend\<Syntactic Category\>I@ functions are not parameterzed by @f@.
--
-- The @idXlate@ function is used by Nanopass to translate @XlateI@ values into @Xlate@ values.
-- This is done so that the same code paths can be used for both pure and 'Applicative' translations.
-- Under the hood, this is done with appropriate wrapping/unwrapping of v'Data.Functor.Identity.Identity', which is a no-op.
--
-- None of the functions defined by this quasiquoter need to be expoted for Nanopass to function.
-- I expect you will not export any of these definitions directly, but instead wrap them into a complete pass, and only export that pass.
--
-- More details and examples are given in the [readme](https://github.com/edemko/nanopass/blob/master/README.md).
--
-- The syntax is:
--
-- >  ⟨Up.Name⟩ :-> ⟨Up.Name⟩
defpass :: QuasiQuoter
defpass = QuasiQuoter (bad "expression") (bad "pattern") (bad "type") go
  where
  go input = do
    sexprs <- case Stupid.parse input of
      Just it -> pure it
      Nothing -> fail "sexpr syntax error"
    case parseDefPass sexprs of
      Right (l1Name, l2Name) -> do
        l1 <- reifyLang l1Name
        l2 <- reifyLang l2Name
        mkXlate l1 l2
      Left err -> fail err
  bad ctx _ = fail $ "`defpass` quasiquoter cannot be used in a " ++ ctx ++ "context,\n\
                     \it can only appear as part of declarations."
  parseDefPass :: [Sexpr String] -> Either String (UpDotName, UpDotName)
  parseDefPass [Atom l1, Atom ":->", Atom l2]
    | Just l1Name <- toUpDotName l1
    , Just l2Name <- toUpDotName l2
      = Right (l1Name, l2Name)
  parseDefPass _ = Left "expecting two language names, separated by :->"
