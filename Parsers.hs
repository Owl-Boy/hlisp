import Control.Applicative as CA
import Control.Monad

import Data.Char

newtype Parser a 
  = Parser { parse :: String -> Either String (a, String)}

runParser :: Parser a -> String -> Either String a
runParser (Parser p) s = fst <$> p s

(<?>) :: Parser a -> String -> Parser a
(Parser p) <?> err = Parser go where
  go s = case p s of
    Left _  -> Left err
    Right x -> return x
infixl 2 <?>

instance Functor Parser where
  fmap fn (Parser p) = Parser go where
    go st = case p st of
      Left e            -> Left e
      Right (res, str') -> Right (fn res, str')

instance Applicative Parser where
  pure x = Parser $ \str -> Right(x, str)
  
  (Parser p) <*> (Parser p') = Parser go where
    go st = case p st of
      Left e          -> Left e
      Right (fn, st') -> case p' st' of
        Left e'         -> Left e'
        Right (v, st'') -> Right (fn v, st'')

instance Alternative Parser where
  empty = fail' ""

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

fail' m = Parser $ \_ -> Left m

any' :: Parser Char
any' = Parser go where
  go []     = Left "any: end of file"
  go (x:xs) = Right (x, xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  x <- any'
  if f x
     then return x
     else fail' "satisfy: does not satisfy"

char :: Char -> Parser Char
char c = satisfy (c ==) <?> "char: expected literal " ++ [c]
oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s) <?> "oneOf: expected one of '" ++ s ++ "'"

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x 
  string xs
  return $ x:xs

-- some :: Parser Char -> Parser String
-- some p = do
--   x <- p
--   xs <- Main.some p
--   return $ x:xs
--   <|> return ""

-- many :: Parser Char -> Parser String
-- many p = do
--   x <- p
--   xs <- Main.some p
--   return $ x:xs

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

lexeme :: Parser a -> Parser a
lexeme = (spaces *>)

reserved :: String -> Parser String
reserved = lexeme . string

natural :: Parser Int
natural = do 
  n <- many digit
  return $ read n

number :: Parser Int
number = do 
  char '-'
  n <- natural
  return (-n)
  <|> natural

between :: Parser b -> Parser c -> Parser a -> Parser a
between o c x = o *> x <* c

contents :: Parser a -> Parser a
contents x = spaces *> x <* spaces
