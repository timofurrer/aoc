module Lib.Parsing where

-- From: https://github.com/nicuveo/advent-of-code/blob/main/2023/haskell/lib/AOC/Parsing.hs

import Text.Parsec
import Text.Parsec.String


-- parsing

-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)
infixr 8 ...

parseWith :: Parser a -> String -> a
parseWith = either (error . show) id ... flip parse ""

parseLinesWith :: Parser a -> String -> [a]
parseLinesWith p s = parseWith p <$> lines s

-- elements

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* spaces

identifier :: Parser String
identifier = lexeme $ many1 lower

number :: Parser Int
number = lexeme $ choice
  [ char '-' *> fmap negate digits
  , char '+' *> digits
  , digits
  ]
  where
    digits = read <$> many1 digit


-- language

symbol :: String -> Parser String
symbol = lexeme . try . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`sepBy` symbol ",")

