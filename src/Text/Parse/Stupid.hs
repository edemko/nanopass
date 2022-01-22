{-# LANGUAGE DeriveFunctor #-}
module Text.Parse.Stupid
  ( parse
  , hydrateSpaces
  ) where
import Data.Bifunctor (first)

data Sexpr a = Atom a | Combo String [Sexpr a]
  deriving (Eq, Ord, Show, Read, Functor)

brackPairs :: [(String, String)]
brackPairs =
  [ ( "("  , ")" )
  , ( "$(" , ")" )
  , ( "["  , "]" )
  , ( "{"  , "}" )
  ]

parse :: String -> Maybe [Sexpr String]
parse = fmap fst . go . tokenize
  where
  go :: [String] -> Maybe ([Sexpr String], [String])
  go [] = Just ([], [])
  go (t:ts) = case t of
    close | close `elem` fmap snd brackPairs -> Just ([], t:ts)
    open | Just close <- lookup open brackPairs -> do
      (inner, rest) <- go ts
      case rest of
        t':rest' | t' == close -> (fmap . first) (Combo open inner :) (go rest')
        _ -> Nothing
    _ -> (fmap . first) (Atom t:) (go ts)

tokenize :: String -> [String]
tokenize input = do
  line <- lines input
  case line of
    '#':_ -> [] -- remove comment lines
    _ -> do
      word <- words line
      unbracket word

unbracket :: String -> [String]
unbracket = filter (not . null) . loop ""
  where
  loop acc "" = [reverse acc]
  loop acc ('$':'(':cs) = reverse acc : "$(" : loop "" cs
  loop acc (c:cs)
    | c `elem` "()[]{}" = reverse acc : [c] : loop "" cs
    | otherwise = loop (c:acc) cs

hydrateSpaces :: String -> String
hydrateSpaces ('\"':content) = go content
  where
  go [] = []
  go ('\\':'\\':rest) = '\\':'\\':go rest
  go ('\\':'+':rest) = ' ':go rest
  go (c:rest) = c:go rest
hydrateSpaces str = str
