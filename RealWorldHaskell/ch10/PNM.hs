
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)


data Greymap = Greymap
    { greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m


matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
    Nothing -> Nothing
    Just (num, rest)
        | num <= 0 -> Nothing
        | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str =
    let count = fromIntegral n
        both@(prefix, _) = L.splitAt count str
    in if L.length prefix < count
        then Nothing
        else Just both
{-
我们的首版最简单的parser如下
parseP5 s = case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 -> case getNat s1 of
        Nothing -> Nothing
        Just ...
不断的产生Maybe然后解构Maybe，嵌套很多层
对于命令式语言来说，使用 if .. return 的方式也还算能看
parseP5 (s) {
    if (matchHeader("P5") ) {
        return null
    }
    var a={width, s2} = getNat(s1)
    if (!a) {
        return null
    }
    ...
}
但在函数式需要采用更好的方法

-}
--parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)

{-
首先我们抽象不断解构Maybe的过程，
可以看到,我们在Just的时候才进行后续处理
-}
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

{-
这样我们的过程可以写成这样
-}
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s
    = matchHeader (L8.pack "P5") s
    >>? \s -> skipSpace ((), s)
    >>? (getNat . snd)
    >>? skipSpace
    >>? \(width, s) -> getNat s
    >>? skipSpace
    >>? \(height, s) -> getNat s
    >>? \(maxGrey, s) -> getBytes 1 s
    >>? (getBytes (width * height) . snd)
    >>? \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

-- go Parse.hs
