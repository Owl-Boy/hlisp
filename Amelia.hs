module Parser
  ( Parser()
  , module X
  , Parser.any
  , satisfy
  , string
  , digit
  , number
  , spaces
  , reserved
  , lexeme
  , (<?>)
  , runParser
  , between ) where

import Control.Applicative as X
import Control.Monad as X

import Data.Char

newtype Parser a
  = Parser { parse :: String -> Either String (a, String) }

runParser :: Parser a -> String -> Either String a
runParser (Parser p) s = fst <$> p s

(<?>) :: Parser a -> String -> Parser a
p <?> err = p <|> Parser.fail err
infixl 2 <?>

instance Functor Parser where
  fn `fmap` (Parser p) = Parser go where
    go st = case p st of
      Left e            -> Left e
      Right (res, str') -> Right (fn res, str')

instance Applicative Parser where
  pure x = Parser $ \str -> Right (x, str)
  (Parser p) <*> (Parser p') = Parser go where
    go st = case p st of
      Left e -> Left e
      Right (fn, st') -> case p' st' of
        Left e' -> Left e'
        Right (v, st'') -> Right (fn v, st'')

instance Alternative Parser where
  empty = Parser.fail "nothing"
  (Parser p) <|> (Parser p') = Parser go where
    go st = case p st of
      Left _  -> p' st
      Right x -> Right x

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser go where
    go s = case p s of
      Left e -> Left e
      Right (x, s') -> parse (f x) s'
fail m = Parser go where
  go = Left . go'
  go' []     = "expected " ++ m ++ ", got to the end of stream"
  go' (x:xs) = "expected " ++ m ++ ", got '" ++ x:"'"


any :: Parser Char
any = Parser go where
  go []     = Left "any: end of file"
  go (x:xs) = Right (x,xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do x <- Parser.any
               if f x
                 then return x
                 else Parser.fail "a solution to the function"


char :: Char -> Parser Char
char c = satisfy (c ==) <?> "literal " ++ [c]

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s) <?> "one of '" ++ s ++ "'"

string :: String -> Parser String
string [] = return []
string (x:xs) = do char   x
                   string xs
                   return $ x:xs

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

reserved :: String -> Parser String
reserved = lexeme . string

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> empty
  cs <- some digit
  return $ read (s ++ cs)

between :: Parser b -> Parser c -> Parser a -> Parser a
between o c x = o *> x <* c

contents :: Parser a -> Parser a
contents x = spaces *> x <* spaces

sep :: Parser b -> Parser a -> Parser [a]
sep s c = sep1 s c <|> return []

sep1 :: Parser b -> Parser a -> Parser [a]
sep1 s c = do x <- c
              xs <- many $ s >> c
              return $ x:xs

option :: a -> Parser a -> Parser a
option x p = p <|> return x

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing $ Just <$> p

optional :: Parser a -> Parser ()
optional p = void p <|> return ()

eof :: Parser ()
eof = Parser go where
  go (x:_) = Left $ "expected eof, got '" ++ x:"'"
  go []    = Right ((), [])
