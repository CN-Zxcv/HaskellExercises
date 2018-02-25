
module Logger (
      Logger
    , Log
    , runLogger
    , record
    ) where


import Text.Regex.Posix ((=~))
import Data.List (elemIndex)
import Data.Ix (inRange)

--globToRegex :: String -> String
--globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex :: String -> Logger String
globToRegex cs = globToRegex' cs >>= \ds -> return ('^':ds)

--globToRegex' :: String -> String
--globToRegex' "" = ""
--globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
--globToRegex' ('?':cs) = '.' : globToRegex' cs
--globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
--globToRegex' ('[':c:cs) = '[' : c :charClass cs
--globToRegex' ('[':_) = error "unterminated character class"
--globToRegex' (c:cs) = escape c ++ globToRegex' cs

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) =
    record "character class, neagetive" >>
    charClass cs >>= \ds ->
    return ("[" ++ c:ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

--escape :: Char -> String
--escape c
--    | c `elem` regexChars = '\\' : [c]
--    | otherwise = [c]
--    where
--        regexChars = "\\+()^$.{}]|"
escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\', c]
    | otherwise = return [c]
    where
        regexChars = "\\+()^$.{}]|"

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i -> return (f i)

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)
--charClass :: String -> String
--charClass (']':cs) = ']' : globToRegex' cs
--charClass (c:cs) = c : charClass cs
--charClass [] = error "unterminated character class"
charClass :: String -> Logger String
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` globToRegex' cs

--matchesGlob :: FilePath -> String -> Bool
--name `matchesGlob` pat = name =~ globToRegex pat

newtype Logger a = Logger {execLogger :: (a, Log)}

type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    m >>= k =
        let (a, w) = runLogger m
            n = k a
            (b, x) = runLogger n
        in Logger (b, w ++ x)
    return a = Logger (a, [])
