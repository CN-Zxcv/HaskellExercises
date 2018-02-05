

module PrettyJSON
    ( renderJValue
    ) where

import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import Numeric (showHex)

import SimpleJSON
import Prettify

-- 直接的，我们从JValue 直接转换到 String
-- 为了使输出适应各种输出形式, 我们增加一个间层，Doc
-- JValue => Doc => String
-- 我们只要重新定义Doc -> String 就可以满足

-- 先从简单的开始
renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

-- 数组和序对对每个元素应用相应的转换函数，然后连接起来 我们使用series
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where
        field (name, val) = string name <> text ": " <> renderJValue val

-- 从定义来说，string即是由""包围的char类型的列表
-- 所以我们需要
-- 转换char到Doc的函数 oneChar
-- 将Doc(Char) 连接成 Doc(String) 的 hcat
-- 给Doc(String) 附加""的 enclose
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

-- 所有字符转换为可显示的
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
    Just r -> text r
    Nothing
        | mustEscape c -> hexEscape c
        | otherwise -> char c
    where
        -- unicode
        mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where
        ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
    where
        h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where
        a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c
    | d < 0x10000 = smallHex d
    | otherwise = astral (d - 0x10000)
    where
        d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item
