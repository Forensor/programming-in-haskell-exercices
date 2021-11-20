module Ch13 where

import Control.Applicative
import Data.Char

-- 1. Define a parser comment :: Parser () for ordinary Haskell comments that begin with
--    the symbol -- and extend to the end of the line with '\n'

newtype Parser a =
  P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item =
  P (\inp ->
       case inp of
         [] -> []
         (x:xs) -> [(x, xs)])

instance Functor Parser where
  fmap g p =
    P
      (\inp ->
         case parse p inp of
           [] -> []
           [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px =
    P
      (\inp ->
         case parse pg inp of
           [] -> []
           [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f =
    P
      (\inp ->
         case parse p inp of
           [] -> []
           [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (const [])
  p <|> q =
    P
      (\inp ->
         case parse p inp of
           [] -> parse q inp
           [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else empty

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x : xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

symbol :: String -> Parser String
symbol xs = token (string xs)

eol :: Char
eol = '\n'

comment :: Parser ()
comment = do
    string "--"
    many (sat (/= eol))
    return ()