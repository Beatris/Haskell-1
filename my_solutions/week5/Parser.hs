module Parser where


import Control.Applicative

newtype Parser a =
  Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
--fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> (\(x, y) -> (f x, y)) <$> p s

instance Applicative Parser where
--pure :: a -> Parser a
--(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pure  a = Parser $ \s -> Just (a, s)
  Parser fs <*> Parser p = Parser $ \s -> case fs s of
                                          Just (f, s') -> parse (fmap f p) s'
                                          Nothing      -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy b = Parser $ \(x:xs) -> if b x then Just (x, xs) else Nothing

char :: Char -> Parser Char
char ch = Parser $ \(x:xs) -> if x == ch then Just (x, xs) else Nothing

openingBrace :: Parser Char
openingBrace = Parser $ \(x:xs) -> if x == '(' then Just (x, xs) else Nothing

closingBrace :: Parser Char
closingBrace = Parser $ \(x:xs) -> if x == ')' then Just (x, xs) else Nothing

inBraces :: Parser a -> Parser a
inBraces p = openingBrace *> p <* closingBrace

instance Alternative Parser where
  empty = undefined
  (<|>) = undefined

oneOrMore, zeroOrMore  :: Parser a -> Parser [a]
oneOrMore = undefined

zeroOrMore = undefined