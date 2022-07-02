# Nanopass in Haskell

The original [Nanopass Compiler Framework](https://nanopass.org/) is an domain-specific language embedded in Racket (Scheme), which aids in the construction of maintainable compilers.
Its advantages stem from its ability to:
  * concisely define a series (in fact, a graph) of slightly-different languages
    by describing _modifications_ to other intermediate languages, and
  * create automatic translations between those languages,
    so that the writer of a compiler pass need not supply the boilerplate of repackaging one type's constructor into another's,
    but can focus on the interesting parts of the pass.
It is suitable for both educational use (students can easily get to the essence of compilation in a few short weeks),
  but also for [production use](http://andykeep.com/pubs/dissertation.pdf).

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
   |  let d* in e
d* ::= x = e
    |  x = e; d*
```
and define a translation from λₗₑₜ to the original λ:
```
⟦let x = eₓ in e⟧ = (λx. e) eₓ
⟦let x = eₓ; d* in e⟧ = (λx. ⟦let d* in e⟧) eₓ
```

Why can't we do this in Haskell?
The main problem is that the author is abusing notation:
  when the syntax looks the same in λₗₑₜ as it does in λ, they just let the reader imagine the injections from one to the other.
Haskell compilers, as smart as they are, are (thankfully?) not smart enough to have human intuition and a sense of the "obvious".
That's why we need to write a bunch of boilerplate… or have Template Haskell write it for us!
Observe the close correspondence of the following Haskell code with the informal mathematics from before.

First, we will define a syntax language of λ.
```haskell
{-# LANGUAGE QuasiQuotes #-}
module Lambda where
import Language.Nanopass (deflang)

[deflang| Lambda
(Expr
  ( Var String )
  ( Lam {x String} {body $Expr} )
  ( App {f $Expr} {a $Expr} )
)
|]
```

Then, in a separate module, we will define λₗₑₜ by modifying our existing λ implementation.
It's best to put each language in its own module.
For one thing, for Nanopass to be useful, many constructor and field names are shared between languages.
On the other, Haskell's compile times are super-linear in the size of a module but (barring full-program optimization) linear in the number of modules; since Template Haskell can generate lots of code, it's broadly good to keep its usage contained.

```haskell
module LambdaLet where
import Language.Nanopass (deflang, defpass)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (foldl1)

import qualified Lambda as L0

[deflang|L0.Lambda :-> LambdaLet
(*
  ( Expr
    (+ Let {bind ({String $Expr} +)} {letIn $Expr} )
  )
)
|]
```

Note that here, we got to define a `NonEmpty` list of tuples using the `({String $Expr} +)`.
Even academic authors sometimes don't avail themselves of such data structures, but we eliminated a syntactic category for free!

```haskell
-- This no-op splice separates the two quasiquotes so that the definitions of the
-- first are available to the second. Declaration order can be finicky, and
-- hopefully I can get rid of this requirement, but for now I've pointed it out
-- because I expect it to be a pitfall for people not familiar with TH. Of course,
-- this is not needed if your pass is defined in a separate module from the
-- language definition.
$(pure [])

[defpass|LambdaLet :-> L0.Lambda|]

compile ∷ ∀ f. Applicative f ⇒ Expr → f L0.Expr
compile = descendExpr xlate
  where
    xlate ∷ Xlate f -- type signature unneeded, but included for explanatory purposes
    xlate = Xlate
      -- the exprLet is required because nanopass couldn't find an automatic translation
      { exprLet = \let' in' → compile $ foldr unlet in' let'
      -- the `expr` member allows us to optionally override the default translation when necessary
      , expr = const Nothing -- we don't need to override anything
      }
      where
        unlet (x, e) body = (Lam x body) `App` e
```

Thankfully, we didn't need to write any code to translate the `Var`, `App`, or `Lam` constructors:
  we could focus on just the important part, which was the `Let` constructor of `Expr`.
Now consider the code savings that such an approach could provide for
  a language with a hundred or more data constructors
  spread across several mutually-recursive types, and which
  must make its way through dozens of passes!

Something I especially enjoy is that all this metaprogramming generates _bog-standard_ Haskell.
The generated code doesn't use any language extensions, and the most sophisticated typeclass it uses is `Traversable`.
The most sophisticated thing we do is pass a record of functions through a recursion, but in all cases this record is defined at the use-site, and so my hope is that inlining and simplification will get rid of any overhead relative to to plain pattern-matching.
My expectation is that the resulting code will be fast because it is the style of code that the compiler most understands.

## The Full Range of Nanopass

The example above only examined a portion of this implementation's capabilities.
Also, examples alone are not good enough to describe a system; one must have definitions as well.

Nanopass generates sets of mutually-recursive types called languages,
  and also functions to help translate between different languages.
We'll first go over the concepts, and then give the syntax.

### Languages

A *language* in Nanopass is represented as a set of mutually-recursive types.
One of these generated types is called a *syntactic category*.
Languages can be parameterized, which means that each syntactic category (one of the mutually-recursive types) is parameterized with the same type variables.
Every syntactic category has one or more constructors, called *productions*.
These productions are records, and each member is called a *subterm*.
If a production only has one subterm, it need not specify a name, and the name `un<Production>` will be used.

Each language is identified by a *language name*.
Under the hood, the language name is also the name of a type with constructors that reference (by name) to the syntactic categories of the language.
Thus, languages names must start with an uppercase letter, and may be qualified.

It is best to define each language in a separate module.
You will need to export the language type (named after the language name) and all its constructors,
  and you will also need to export each syntactic category (and its constructors).
If the only thing you define in a module is a language, then it's easy enough to just export everything.

### Translations

You can request Nanopass to generate automatic translation between two languages.
However, the common case is that some language terms cannot be automatically translated, and you may also need to do something different from the automatic translation.
Thus, the generated functions are parameterized by a type named `Xlate`, which has a member for each
  1. *hole*, which is a production in the source language which is altered or missing in the target, and
  2. *override*, which is a syntactic category with the same name in both languages.
This type assumes the translation will occur in an `Applicative` functor.

A translation function is generated for each syntactic category with the same name in both source and target languages.
The name of the translation function is `descend<Syntactic Category>`.
At the moment, there is no provision for altering the name of the type or translation function(s),
  but I expect you'll only want to define one translation per module.
The type of a `descend<Syntactic Category>` function is
  `Xlate f → σ → f σ'`.

The `Xlate` type takes all the parameters from both languages (de-duplicating parameters of the same name),
  as well as an additional type parameter, which is the functor `f` under which the translation occurs.

If a production in the source language has subterms `τ₁ … τₙ` and is part of the syntactic category `σ`,
  then a hole member is a function of type `τ₁ → … τₙ → f σ'`, where `σ'` is the corresponding syntactic category in the target language.
Essentially, you get access all the subterms, and can use the `Applicative` to generate a target term as long as you don't cross syntactic categories.

If a source language has syntactic category `σ` with the same name as the target's syntactic category `σ'`,
  then an override member is a function of type `σ → Maybe (f σ')`.
If an override returns `Nothing`, then the automatic translation will be used,
  otherwise the automatic translation is ignored in favor of the result under the `Just`.

We also generate a pure variant of the functor-based translations.
The differences are:
  * The type `XlateI` is generated; it is not parameterized by `f`, nor are the types of its members.
  * The members of `XlateI` are the same as for `Xlate`, but suffixed with the letter `I`.
  * The pure descend functions are named `descend<Syntactic Category>I`.
    They take an `XlateI` instead of an `Xlate`, and return their results directly rather than under an `Applicative`.
  * A function `idXlate` is generated, which takes values of `XlateI` to `Xlate`.
    This is only used internally so that the same code paths can be used for both pure and `Applicative` translations.
    Under the hood, this is done with appropriate wrapping/unwrapping of `Identity`, which is a no-op.

So, what _can_ be auto-translated?
If the subterms of a production don't match, there's nothing we can do, but even when they do match, we can't always generate a translation.
Broadly, a subterm can be auto-translated when it mentions other syntactic categories only in `Traversable` position.
  * An auto-translation exists for any subterm which has a type that corresponds to a syntactic category in the target languatge
  * A trivial auto-translation exists when the subterm does not mention any other syntactic categories
  * An auto-translation knows about tuples: as long as every element of the tuple is translatable, the tuple is translatable
  * An auto-translation knows about `Traversable`:
    if the only mention of a syntactic category is in the last type argument of a type constructor
      and that type has a `Traversable` instance, we translate using `traverse`.
    Importantly, this includes common data structures useful for defining languages,
      such as lists, non-empty lists, `Maybe`, and `Map k` when `k` does not mention a syntactic category.


I had considered just calling `error` when the automatic translation couldn't be generated.
However, this would lead to functions like `case term of { … ; _ -> defaultXlate }`, which hide incomplete pattern match warnings.
By using an `Xlate` type, we maintain warnings whenever part of the translation is not defined; it's just that those warnings are uninitialized member warnings instead.

### Syntax

We embed the syntax of the quasiquoters in a modified form of sexprs which allow---and distinguish between---square and curly brackets alongside round brackets.
Atoms are just sequences of characters that don't contain whitespace, though we only recognize a handful of these as valid syntactically.
Importantly, we treat symbols differently based on their shape:
  * `UpCamelCase` is used as in normal Haskell: to identify constructors, both type- and data-
  * `$Name` is used for recursive references
  * `lowerCamel` is used for language parameters and the names of terms
  * `DotSeparated.UpCamelCase` is used to qualify the names of languages and types.
  * a handful of operators are used

Since the syntax is based on s-expressions, we use [Scheme's entry format](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-4.html#%_sec_1.3.3) conventions for describing the syntax.
Importantly, we syntactic variables are enclosed in `⟨angle brackets⟩`, and ellipsis `⟨thing⟩…` indicate zero or more repetitions of `⟨thing⟩`.
Round, square, and curly brackets, as well as question mark, asterisk, and so on have no special meaning: they only denote themselves.

The syntax for defining languages, from scratch or by derivation is:
```
langdef
  ::= ⟨language definition⟩
   |  ⟨language modification⟩

language definition
  ::= ⟨UpName⟩ ( ⟨lowName⟩… ) ⟨syntactic category⟩…
  ::= ⟨UpName⟩ ⟨syntactic category⟩…

language modification
  ::= ⟨Up.Name⟩ :-> ⟨UpName⟩ ( ⟨lowName⟩… ) ⟨syntactic category modifier⟩…
   |  ⟨Up.Name⟩ :-> ⟨UpName⟩ ⟨syntactic category modifier⟩…

syntactic category ::= ( ⟨UpName⟩ ⟨production⟩… )
production ::= ( ⟨UpName⟩ ⟨subterm⟩… )
subterm
  ::= { ⟨lowName⟩ ⟨type⟩ }
   |  ⟨type⟩

type
  ::= $⟨UpName⟩                               # reference a syntactic category
   |  ⟨lowName⟩                               # type parameter
   |  ( ⟨Up.Name⟩ ⟨type⟩… )                   # apply a Haskell Type constructor to arguments
   |  ⟨Up.Name⟩                               # same as: (⟨UpName⟩)
   |  ( ⟨type⟩ ⟨type operator⟩… )             # apply common type operators (left-associative)
   |  ( ⟨Up.Name⟩ ⟨type⟩… ⟨type operator⟩… )  # same as: ((⟨UpName⟩ ⟨type⟩…) ⟨type operator⟩…)
   |  { ⟨type⟩ ⟨type⟩ ⟨type⟩… }               # tuple type
   |  [ ⟨type⟩ :-> ⟨type⟩ ]                   # association list: ({⟨type⟩ ⟨type⟩} *)
   |  { ⟨type⟩ :-> ⟨type⟩ }                   # Data.Map

type operator
  ::= *  # []
   |  +  # NonEmpty
   |  ?  # Maybe

syntactic category modifier
  ::= ( + ⟨syntactic category⟩… )   # add
   |  ( - ⟨UpName⟩… )               # remove
   |  ( * ⟨production modifier⟩… )  # modify
production modifier
  ::= ( + ⟨UpName⟩ ⟨subterm⟩… )
   |  ( - ⟨Upname⟩ )
```

The syntax for requesting a translation is:
```
⟨Up.Name⟩ :-> ⟨Up.Name⟩
```

### What are "Syntactic Categories"?

In Nanopass, the line between terminal and non-terminal is blurred, perhaps even erased out of existence.
Context-free grammars can make a clear distinction because they require non-terminals to appear simpliciter in the string of symbols on the rhs of a production.
In contrast, informal descriptions of abstract grammars often use notational convenience---such as list or finite map comprehensions---in defining grammars.

It's easy to see that the mutually-recursive types defined by the grammar (e.g. `Expr`, `Stmt`) correspond to the notion of non-terminals, and types which have previously been defined (`Int`, `[Char]`) correspond to terminals.
However, there is no technical reason to disallow types such as `[Expr]` (or far more exotic types), where the type constructor has already been defined (like a terminal), but supplied with one of the language's types (like a non-terminal).
Incidentally, the fact that this just works™ lends some credibility to its appearance in the informal definitions common in the academic literature.

Rather than attempt to carve out new, subtle terms, we've decided on "syntactic category" as a catch-all term for terminals, non-terminals and anything in-between.
This term is [already established in the field of linguistics](https://en.wikipedia.org/wiki/Syntactic_category).
At least some\* theories of natural language grammar use the term to collect both lexical categories (which correspond to terminals) and phrasal categories (which correspond to non-terminals), and indeed this is where linguistics and computer science come very close to intersection.
(After all, the Chomsky Hierarchy we learn in a foundations of computation course is named after linguist Noam Chomsky, who made significant contributions to phrase structure grammar, including coining the term.)

\*Some other theories dispense entirely with the concept of a phrase, making use of the term moot.

Admittedly, "syntactic category" is a mouthful (and a keebful), so in the code I often abbreviate to `syncat`.
If `syncat` makes its way into user-facing documentation, that is a bug and should be reported.
Good technical writing demands that fragments of text be reasonably understandable in isolation, and custom portmanteaus don't help.
