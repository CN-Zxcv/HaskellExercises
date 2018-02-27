
{-
Parser for application/x-www-form-urlencoded

foo=bar&a%21=b+c

-}

import Text.ParserCombinators.Parsec
import Numeric

-- query 由 & 分隔的pair组成
p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

-- pair 由 = 分隔的(key, value) 组成
p_pair :: CharParser () (String, Maybe String)
p_pair = do
    name <- many1 p_char
    value <- optionMaybe (char '=' >> many p_char)
    return (name, value)

p_char :: CharParser () Char
p_char = noneOf urlBaseChars
    <|> (char '+' >> return ' ')
    <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a,b]
    return . toEnum $ d
