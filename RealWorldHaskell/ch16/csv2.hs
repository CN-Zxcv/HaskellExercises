
import Text.ParserCombinators.Parsec

-- 通过endBy sepBy 简化csv1.hs的parser
--csvFile = endBy line eol
--line = sepBy cell (char ',')
--cell = many (noneOf ",\n")
--eol = char '\n'
--
--parseCSV :: String -> Either ParseError [[String]]
--parseCSV input = parse csvFile "(unknown)" input

{-
尝试解决多种换行的支持 "\n" "\n\r" "\r"

-- <|> 在第一个不消耗输入的时候才选择第二个，所以"\r"必然无法被消耗掉
eol1 :: GenParser Char st String
eol1 = string "\n" <|> string "\n\r"

-- 因为string "\n\r" 在对比第一个字符'\n'就消耗掉了字符，
-- 所以<|>选择了第一个,
-- string "\n\r" 无法正确匹配，会导致报错
eol2 :: GenParser Char st String
eol2 = string "\n\r" <|> string "\n"

eol3 :: GenParser Char st Char
eol3 = do
    char '\n'
    char '\r' <|> return '\n'

这里return的作用??
return 任意字符好像都是可以的

-- try 函数
-- try 函数放在<|>左侧时才有效,所以要先写try后写其他的
eol4 :: GenParser Char st String
eol4 = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> fail "Couldn`t find EOL"

-- fail 多种可能的时候,默认错误提示可能会很丑,所以提供了自定义错误信息

-- <|> fail "what" 追加错误信息
eol5 :: GenParser Char st String
eol5 = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> fail "Couldn`t find EOL"

-- <?> "what" 替换错误信息
eol6 :: GenParser Char st String
eol6 = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "End of line"
-}

{-
完整的parser
-}

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r
