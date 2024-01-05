# Nanopass in Haskell

The original [Nanopass Compiler Framework](https://nanopass.org/) is a domain-specific language
embedded in Racket (a Scheme dialect), which aids in the construction of maintainable compilers.
Its advantages stem from its ability to:
  * concisely define a series (in fact, a graph) of slightly-different languages
    by describing _modifications_ to other intermediate languages, and
  * create automatic translations between those languages,
    so that the writer of a compiler pass need not supply the boilerplate of repackaging one type's constructor into another's,
    but can focus on the interesting parts of the pass.
It is suitable for both educational use (students can easily get to the essence of compilation in a few short weeks),
  but also for [production use](http://andykeep.com/pubs/dissertation.pdf),
  and—I would add—particularly suited to exploring language design space.

In contrast, the best choices available for compiler writers in Haskell require finding a balance between the unreliability induced by moving invariants out of the type system (as in GHC before implementing Trees that Grow), writing significant boilerplate (even the [Trees that Grow](https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf) approach is significantly more verbose and unnatural than Nanopass), and risking low performance (such as when using generics).

I have envied Nanopass for its elegance, but didn't want to give up static typing for it.
Not anymore!
Today, I actually know enough Template Haskell that it has become possible for me to port Nanopass into Haskell, and that is what this is.

## A Small Example

Let's say we find an academic paper that describes the syntax of a simple lambda calculus:
```
e ::= x
   |  λx. e
   |  e₁ e₂
```
Then the author goes on to describe let-binding as syntactic sugar.
They make the relevant changes to the grammar:
```
e ::= …
   |  let d in e
d ::= x = e
   |  x = e; d
```
and define a translation from λₗₑₜ to the original λ:
```
⟦let x = eₓ in e⟧ = (λx. e) eₓ
⟦let x = eₓ; d in e⟧ = (λx. ⟦let d in e⟧) eₓ
```

Why can't we do this in Haskell?
The main problem is that the author is abusing notation:
  when the syntax looks the same in λₗₑₜ as it does in λ, they just let the reader imagine the injections from one to the other.
Haskell compilers, as smart as they are, are (thankfully?) not smart enough to have human intuition and a sense of the "obvious".
That's why we need to write a bunch of boilerplate… or have Template Haskell write it for us!
Observe the close correspondence of the following Haskell code with the informal mathematics from before.

First, we will define a syntax language of λ.
```
{-# LANGUAGE QuasiQuotes #-}
module Lambda where
import Language.Nanopass (deflang)

[deflang|
(Lambda
  (Expr
    (Var String)
    (Lam String ($ Expr))
    (App ($ Expr) ($ Expr))
  )
)|]
```
Each recursive call back into a language non-terminal must be prefixed with the `$` operator
to distinguish it from ordinary Haskell data types.

Then, in a separate module, we will define λₗₑₜ by modifying our existing λ implementation.
It's best to put each language in its own module.
For one thing, for Nanopass to be useful, many constructor and field names are shared between languages.
On the other, Haskell's compile times are super-linear in the size of a module but (barring full-program optimization) linear in the number of modules; since Template Haskell can generate lots of code, it's broadly good to keep its usage contained.

```
module LambdaLet where
import Language.Nanopass (deflang, defpass)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (foldl1)

import qualified Lambda as L0

[deflang|
(LambdaLet from L0.Lambda
  (* Expr
    (+ Let
      (+ (& String ($ Expr)))
      ($ Expr)
    )
  )
)|]
```
This says that we will modify the `(* Expr …)` non-terminal by adding a production `(+ Let …)`.
Note that in `(+ (& String ($ Expr)))`, we defined a `NonEmpty` with the `+` operator, and a tuple with the `&` operator.
Even academic authors sometimes don't avail themselves of such data structures, but we eliminated a non-terminal for free!

```
-- This no-op splice separates the two quasiquotes so that the definitions of the
-- first are available to the second. Declaration order can be finicky, and
-- hopefully I can get rid of this requirement, but for now I've pointed it out
-- because I expect it to be a pitfall for people not familiar with TH. Of course,
-- this is not needed if your pass is defined in a separate module from the
-- language definition.
$(pure [])

[defpass|LambdaLet :-> Lambda|]

compile :: L0.Expr -> Expr
compile = runIdentity . descendExpr xlate
  where
  xlate :: Xlate Identity -- type signature unneeded, but included for explanatory purposes
  xlate = Xlate
    -- the onExprLet is required because nanopass couldn't find an automatic translation
    { onExprLet = \binds body -> pure $ foldr unlet body binds
    -- the `onExpr` member allows us to optionally override the default translation when necessary
    , onExpr = const Nothing -- we don't need to override anything
    }
  unlet body (x, e) = (Lam x body) `App` e
```

Thankfully, we didn't need to write any code to translate the `Var`, `App`, or `Lam` constructors:
  we could focus on just the important part, which was the `Let` constructor of `Expr`.
Now consider the code savings that such an approach could provide for
  a language with a hundred or more data constructors
  spread across several mutually-recursive types, and which
  must make its way through dozens of passes!

Something I especially enjoy is that all this metaprogramming generates _bog-standard_ Haskell.
The generated code doesn't use any language extensions, and the most sophisticated typeclass it uses is `Traversable`.
The most sophisticated code we generate is to pass a record of functions through a recursion,
  but in practical use, this record is defined near enough to the use-site that
  my hope is that inlining and simplification will get rid of any overhead relative to to plain pattern-matching.
My expectation is that the resulting code will be fast because it is the style of code that the compiler most understands.

## The Full Range of Nanopass

The example above only examined a portion of this implementation's capabilities.
Also, examples alone are not good enough to describe a system; one must have definitions as well.

Nanopass generates sets of mutually-recursive types called languages,
  and also, separately, functions to help translate between different languages.
We'll first go over the concepts, and then give the syntax.

### Languages

A *language* in Nanopass is represented as a set of mutually-recursive types.
One of these generated types is called a *non-terminal*.
Languages can be parameterized, which means that each non-terminal is parameterized with the same type variables.
Every non-terminal has one or more constructors, called *productions*.
As each production is just an ordinary Haskell data constructor, they can take arguments, which are called *subterms*.
The subterms can be any sort of Haskell data type, but if it is not a non-terminal, then one could call it a terminal.

Each language is identified by a *language name*.
Under the hood, the language name is also the name of a type with constructors that reference (by name) the non-terminals of the language.
Thus, language names must start with an uppercase letter, and may be qualified.

It is best to define each language in a separate module,
  because they commonly share names of data constructors.
You will need to export the language type (named after the language name) and all its constructors,
  and you will also need to export each non-terminal (and its constructors).
If the only thing you define in a module is a language, then it's easy enough to just export everything.

### Translations

You can request Nanopass to generate automatic translation between two languages.
However, the common case is that some language terms cannot be automatically translated, and you may also need to do something different from the automatic translation.
Thus, the generated functions are parameterized by a type named `Xlate`, which has a member for each
  1. *hole*, which is a production in the source language which is altered or missing in the target, and
  2. *override*, which is a non-terminal with the same name in both languages.
This type assumes the translation will occur in an `Applicative` functor.

A translation function is generated for each non-terminal with the same name in both source and target languages.
The name of the translation function is `descend<NonTerminal>`.
At the moment, there is no provision for altering the name of the type or translation function(s),
  but I expect you'll only want to define one translation per module.
The type of a `descend<NonTerminal>` function is
  `Xlate f → σ → f σ'`.

The `Xlate` type takes all the parameters from both languages (de-duplicating parameters of the same name),
  as well as an additional type parameter, which is the functor `f` under which the translation occurs.

If a production in the source language has subterms `τ₁ … τₙ` and is part of the non-terminal `σ`,
  then a hole member is a function of type `τ₁ → … τₙ → f σ'`, where `σ'` is the corresponding non-terminal in the target language.
Essentially, you get access all the subterms, and can use the `Applicative` to generate a target term as long as you don't change the non-terminal type.

If a source language has non-terminal `σ` with the same name as the target's non-terminal `σ'`,
  then an override member is a function of type `σ → Maybe (f σ')`.
If an override returns `Nothing`, then the automatic translation will be used,
  otherwise the automatic translation is ignored in favor of the result under the `Just`.

We also generate a pure variant of the functor-based translations.
The differences are:
  * The type `XlateI` is generated; it is not parameterized by `f`, nor are the types of its members.
  * The members of `XlateI` are the same as for `Xlate`, but suffixed with the letter `I`.
  * The pure descend functions are named `descend<NonTerminal>I`.
    They take an `XlateI` instead of an `Xlate`, and return their results directly rather than under an `Applicative`.
  * A function `idXlate` is generated, which takes values of `XlateI` to `Xlate`.
    This is only used internally so that the same code paths can be used for both pure and `Applicative` translations.
    Under the hood, this is done with appropriate wrapping/unwrapping of `Identity`, which is a no-op.

So, what _can_ be auto-translated?
If the subterms of a production don't match, there's nothing we can do, but even when they do match, we can't always generate a translation.
Broadly, a subterm can be auto-translated when it mentions other syntactic categories only in `Traversable` position.
  * An auto-translation exists for any subterm which has a type that corresponds to a non-terminal in the target language.
  * A trivial auto-translation exists when the subterm does not mention any other non-terminals.
  * An auto-translation knows about tuples: as long as every element of the tuple is translatable, the tuple is translatable.
  * An auto-translation knows about `Traversable`:
    if the only mention of a non-terminal is in the last type argument of a type constructor,
      and that type has a `Traversable` instance, we translate using `traverse`.
    Importantly, this includes common data structures useful for defining languages,
      such as lists, non-empty lists, `Maybe`, and `Map k` when `k` does not mention a non-terminal.

I had considered just calling `error` when the automatic translation couldn't be generated.
However, this would lead to functions like `case term of { … ; _ -> defaultXlate }`, which hide incomplete pattern match warnings.
By using an `Xlate` type, we maintain error detection whenever part of the translation is not defined; it's just that those error are uninitialized strict member errors instead.

### Syntax

The syntax in of the quasiquoters is based on s-expressions.
The documentation in `Nanopass.Internal.Parser` is the primary source of truth for how these s-expressions are interpreted,
  but I will replicate them here (just be warned that the readme may fall out of date).

Of particular note is that s-expressions do not allow dot in a name (as in Haskell qualified names).
Thus, to qualify a name, use a colon instead (`T:Text` in the s-expressions will translate to `T.Text` in Haskell).


The syntax for requesting a translation is: (TODO this will change in the future)
```
⟨Up.Name⟩ :-> ⟨Up.Name⟩
```

The syntax for defining a language (base or modified) is:
```
Language ::= <BaseLang> | <LangMod>

BaseLang ::=
 (<LangLHS>        language name and type variables
     <string…>     documentation
     <Nonterm…>)   syntactic categories

LangMod ::=
 (<LangLHS>             new language name and type variables
       'from'           keyword
       <UpColon>        base language name
     <string…>          documentation
     <NontermsEdit…>)   changes to the base language's syntactic categories

LangLHS ::= <UpCase>                 language name, zero type variables
        |  (<UpCase> <LowCase…>)    language name, type variables

------------------------------
------ Base Definitions ------
------------------------------

Nonterm ::=
 (<UpCase>             type name
     <string…>         documentation
     <Production…>)    constructor arguments

Production ::=
 (<UpCase>        constructor name
     <string…>    documentation
     <Type…>)     constructor arguments

Type ::= ('$' <UpCase name>)            non-terminal (language parameters already applied)
     |  <lowCase name>                 type parameter
     |  <UpColonName>                  plain Haskell type (kind *)
     |  (<UpColonName> <Type…>)        plain Haskell type application
     |  ('?' <Type>)                   Maybe type
     |  ('*' <Type>)                   List type
     |  ('+' <Type>)                   NonEmpty type
     |  () | ('&')                     unit type
     |  ('&' <Type>)                   Only type TODO
     |  ('&' <Type> <Type> <Type…>)    tuple types

---------------------------
------ Modifications ------
---------------------------

NontermsEdit
 ::= ('+'                       add a syntactic category
         <UpCase>                 new non-terminal name
         <string…>                documentation
         <Production…>)           constructors
  |  ('-' <UpCase>)             remove a syntactic category by name
  |  ('*'                       modify a syntactic category's productions
         <UpCase name>            name of non-terminal to edit
         <ProductionsEdit…>)      changes to the base language's non-terminal

ProductionsEdit
 ::= ('+'              add a production
         <UpCase>        new constructor name
         <string…>       documentation
         <Type…>)        constructor arguments
  |  ('-' <UpCase>)    remove a production by name

-------------------
------ Names ------
-------------------

LowCase = /[a-z][a-zA-Z0-9_]/
UpCase = /[A-Z][a-zA-Z0-9_]/
UpColonCase = /[A-Z][a-zA-Z0-9_](:[A-Z][a-zA-Z0-9_])*/
```
