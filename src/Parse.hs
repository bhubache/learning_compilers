module Parse where

import Control.Applicative
import Data.Char
import System.IO as SysIO

parse :: String -> Maybe (String, Program)
parse content =
  let
    ast = runParser program content
   in
    case ast of
      Just ([], _) -> ast
      Just (rest, _)
        | all isSpace rest -> ast
        | otherwise -> Nothing
      _ -> Nothing

newtype Program = Program Function
  deriving (Show)

data Function = Function ReturnType Identifier Params Body
  deriving (Show)

newtype ReturnType = ReturnType String
  deriving (Show)

newtype Identifier = Identifier String
  deriving (Show)

data Params
  = ParamList [Param]
  | Void
  deriving (Show)

data Param = Param CType Identifier
  deriving (Show)

newtype CType = CType String
  deriving (Show)

newtype Body = ReturnStatement Expression
  deriving (Show)

data Expression
  = WrappedExpression String
  | Constant Integer
  deriving (Show)

program :: Parser Program
program = Program <$> function

function :: Parser Function
function = Function <$> returnType <*> identifier <*> paramList <*> body

ctype :: Parser CType
ctype = CType <$> stringP "int"

identifier :: Parser Identifier
identifier = Identifier <$> spanP isAlpha

constant :: Parser Expression
constant = f <$> spanP isDigit
  where
    f digits = Constant $ read digits

-- TODO: Reuse `identifier`
returnType :: Parser ReturnType
returnType = ReturnType <$> (whitespace *> stringP "int" <* whitespace)

paramList :: Parser Params
paramList = Void <$ (whitespace *> charP '(' *> whitespace *> stringP "void" <* whitespace <* charP ')') <* whitespace

param :: Parser Param
param = undefined

body :: Parser Body
body = ReturnStatement <$> (whitespace *> charP '{' *> whitespace *> line <* whitespace <* charP '}')
  where
    line = stringP "return" *> whitespace *> expression <* whitespace <* charP ';'

expression :: Parser Expression
expression = Parser $ \input ->
  case runParser constant input of
    Just (rest, Constant output) -> Just (rest, Constant output)
    Nothing ->
      case runParser identifier input of
        Just (rest, Identifier output) -> Just (rest, WrappedExpression output)
        Nothing -> Nothing

whitespace :: Parser String
whitespace = spanP isSpace

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x : xs)
      | x == c = Just (xs, c)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let
    (token, rest) = span f input
   in
    Just (rest, token)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (rest, output) -> Just (rest, f output)
      _ -> Nothing

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)

  Parser p1 <*> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing -> Nothing
      Just (rest, f) ->
        case p2 rest of
          Nothing -> Nothing
          Just (rest', output) -> Just (rest', f output)

instance Alternative Parser where
  empty = Parser $ const Nothing

  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input
