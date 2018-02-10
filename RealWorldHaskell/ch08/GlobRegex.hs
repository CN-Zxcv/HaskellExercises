
module GlobRegex
    ( globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))
import Data.List (elemIndex)
import Data.Ix (inRange)

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c :charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c
    | c `elem` regexChars = '\\' : [c]
    | otherwise = [c]
    where
        regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat


{-

* :: 匹配所有字符
    Law*
        | Success = Law, Laws,
        | Failure = GrokLaw
    *Law*
        | Success = Law, GrokLaw, Lawyer
        | Failure = La, ow

? :: 匹配一个字符
    ?at
        | Success = Cat, cat, Bat, bat
        | Failure = at
[abc] :: 匹配其中的一个字符
    [CB]at
        | Success = Cat, Bat
        | Failure = cat, bat
[a-z] :: 匹配之间的一个字符
    [a-c]
        | Success = a, b, c
        | Failure = d

-- Posix Only --

[!abc] :: 排除
[!a-z]

以上所有匹配都忽略路径分割符 '\' 或者 '/'

-}

-- 以下实现还有问题
-- Failure for: ("f]c","f[]ac]c",True,False)
-- Failure for: ("a.c","[]-}].c",True,False)
-- Failure for: ("]","[!]]",False,True)

glob :: String -> String -> Bool
glob "" "" = True
glob _ "" = False
glob "" ('*':cs) = cs == []
glob str@(x:xs) pat@('*':cs) = case glob str cs of
    True -> True
    False -> glob xs pat
glob (x:xs) ('?':cs) = glob xs cs
glob xs ('[':'!':cs) = not $ matchesClass xs cs
glob xs ('[':cs) = matchesClass xs cs
glob (x:xs) (c:cs)
    | x == c = glob xs cs
    | otherwise = False

matchesClass :: String -> String -> Bool
matchesClass (x:xs) (']':cs) = False
--matchesClass (x:xs) (']':cs)
--    | x == ']' = matchesClass xs cs
--    | otherwise = False
matchesClass str@(x:xs) pat@(a:'-':b:cs)
    | b == ']' = case x == a of
        True -> glob xs cs
        False -> matchesClass str (tail pat)
    | otherwise = case a < x && x < b of
        True -> glob xs (matchesTail cs)
        False -> matchesClass str cs
matchesClass str@(x:xs) (c:cs) = case x == c of
    True -> glob xs (matchesTail cs)
    False -> matchesClass str cs

matchesTail :: String -> String
matchesTail xs = tail $ dropWhile (/=']') xs

test name pat expected =
    putStr $ if actual == expected then "" else "Failure for: " ++ show (name, pat, expected, actual) ++ "\n"
    where actual = glob name pat

simpleCheck = do
    test "foo.c" "foo.c" True
    test "foo.x" "foo.c" False
    test "foo.x" "foo.?" True
    test "foo.c" "?oo.c" True
    test "foo.c" "?oo.x" False
    test "foo.x" "foo.*" True
    test "foo.x" "*.x" True
    test "foo.x" "f*.x" True
    test "foo.x" "f*.*" True
    test "foo.x" "f*.c" False
    test "foo.x" "f*.[cx]" True
    test "foo.x" "f*.[cy]" False
    test "FOO.x" "[Ff][Oo][oO].[xX]" True
    test "foo.x" "f*.[!cx]" False
    test "foo.x" "f*.[!cy]" True
    test "foo-c" "f*[-abc]c" True
    test "f-c" "f[-ab]c" True
    test "f-c" "f[abc]c" False
    test "f-c" "f[a-c]c" False
    test "fbc" "f[a-c]c" True
    test "f]c" "f[]ac]c" True
    test "a.c" "[]-}].c" True
    test "a-c]"  "[ab]-c]" True
    test "d"  "[ab]-}]" False
    test "a-}]"  "[ab]-}]" True
    test "a" "[!]]" True
    test "]" "[!]]" False
