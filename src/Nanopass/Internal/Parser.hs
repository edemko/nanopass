{-# LANGUAGE OverloadedRecordDot #-}

module Nanopass.Internal.Parser
  ( getSexpr
  , Loc(..)
  ) where

import Text.Megaparsec (runParser',State(..),PosState(..),SourcePos(..),errorBundlePretty)
import Text.Megaparsec.Pos (defaultTabWidth,mkPos)
import Text.SExpression (SExpr,parseSExpr,def)

------ Parse S-Expressions ------

-- | This is a location type that should be sufficient to describe the start of a Template Haskell quasiquote.
-- It is used in 'getSexpr' so that it can report errors from the actual source code location.
data Loc = Loc
  { file :: FilePath
  , line :: Int -- ^ TODO hopefully 1-indexed
  , col :: Int -- ^ TODO hopefully 1-indexed
  }

-- | This serves as an adapter between Template Haskell and whatever s-expression parser I decide to use.
getSexpr :: (Loc, String) -> Either String SExpr
getSexpr (loc, inp) = case runParser' (parseSExpr def) state0 of
    (_, Left err) -> Left $ errorBundlePretty err
    (_, Right sexpr) -> Right sexpr
  where
  state0 = State
    { stateInput = inp
    , stateOffset = 0
    , statePosState = PosState
      { pstateInput = inp
      , pstateOffset = 0
      , pstateSourcePos = SourcePos
        { sourceName = loc.file
        , sourceLine = mkPos loc.line
        , sourceColumn = mkPos loc.col
        }
      , pstateTabWidth = defaultTabWidth
      , pstateLinePrefix = ""
      }
    , stateParseErrors = []
    }

------ Recognize Base Languages ------
