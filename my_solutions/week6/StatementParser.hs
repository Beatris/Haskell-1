module StatementParser where

import Parser
import Statement
import Control.Applicative
import qualified Data.Char as C
  ( isAlpha
  , isDigit
  )


plusP :: Parser Oper
plusP = const Plus <$> char '+'

minusP :: Parser Oper
minusP = const Minus <$> char '-'

multP :: Parser Oper
multP = const Mult <$> char '*'

divP :: Parser Oper
divP = const Div <$> char '/'

modP :: Parser Oper
modP = const Mod <$> char '%'

gtP :: Parser Oper
gtP = const Gt <$> str ">"

geP :: Parser Oper
geP = const Ge <$> str ">="

ltP :: Parser Oper
ltP = const Lt <$> str "<"

leP :: Parser Oper
leP = const Le <$> str "<="

eqlP :: Parser Oper
eqlP = const Eql <$> char '='

oper :: Parser Oper
oper = plusP <|> minusP <|> multP <|> divP <|> modP <|> gtP <|> geP <|> ltP <|> leP <|> eqlP

variable :: Parser Variable
variable = oneOrMore $ satisfy (C.isAlpha)

value :: Parser Value
value =  read <$> (oneOrMore $ satisfy (C.isDigit))

expr :: Parser Expr
expr = Var <$> variable
   <|> Val <$> value
   <|> liftA3 Op expr oper expr

assignP :: Parser Statement
assignP = liftA2 Assign (variable <* char '=') (expr)

incrP :: Parser Statement
incrP = Incr <$> (str "++" *> variable)

decrP :: Parser Statement
decrP = Decr <$> (str "--" *> variable)

ifP :: Parser Statement
ifP = liftA2 If (str "if" *> inBraces expr) (char '{' *> statement <* char '}')

forP :: Parser Statement
forP = (liftA3 For (str "for(" *> statement) (char ';' *> expr <* char ';') (statement <* str "){")) <*> (statement <* char '}')

statement :: Parser Statement
statement = assignP <|> incrP <|> decrP <|> ifP <|> forP

interpret :: Parser [Statement]
interpret = oneOrMore statement