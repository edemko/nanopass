module Lang where

import Language.Nanopass.LangDef

lang :: LangDef
lang = LangDef
  [ ("Expr"
    , [ CtorDef "Var"
        [ (Just "x", CtorType "String" [])
        ]
      , CtorDef "Lam"
        [ (Just "x", CtorType "String" [])
        , (Just "e", ListType (GrammarType "Stmt"))
        ]
      , CtorDef "App"
        [ (Just "f", GrammarType "Expr")
        , (Just "a", GrammarType "Expr")
        ]
      ]
    )
  , ("Stmt"
    , [ CtorDef "Expr"
        [ (Nothing, GrammarType "Expr")
        ]
      , CtorDef "Let"
        [ (Just "x", CtorType "String" [])
        , (Just "e", GrammarType "Expr")
        ]
      ]
    )
  ]
