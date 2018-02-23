
{-
条形码 EAN-13
123456789012
[1..2] : 数字系统，标志国籍啊什么的大类
[3..7] : 厂商id
[8..12] : 产品id
[13] : 校验位

厂商id + 产品id 总共10位，可以适当调整，比如 4 + 6 的样子等

最后的校验位是通过算法计算的(从右往左，奇数 *3 偶数保留, 10 - 所得数字的和的尾数)

条形码图形编码
    黑条表示1 白条表示0,每位一个像素

    101 开头
    6位数字，每个7像素宽(根据第一个数值决定编码方式)
    01010分段
    另外6个数,也是7像素
    101 结尾
    校验位隐藏


    左边6位用奇偶校验位编码，
-}

import Data.Array (Array(..), (!), bounds, elems, indices,
                   ixmap, listArray)

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

-- ch10
import Parse

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

leftOddList =
    [ "0001101"
    , "0011001"
    , "0010011"
    , "0111101"
    , "0100011"
    , "0110001"
    , "0101111"
    , "0111011"
    , "0110111"
    , "0001011"
    ]

rightList = map complement <$> leftOddList
    where
        complement '0' = '1'
        complement '1' = '0'

leftEvenList = map reverse rightList

parityList =
    [ "111111"
    , "110100"
    , "110010"
    , "110001"
    , "101100"
    , "100110"
    , "100011"
    , "101010"
    , "101001"
    , "100101"
    ]

-- array 提供常数复杂度随机访问元素
listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l - 1) xs
    where
        l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s = foldl' f s . elems

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f = foldl1 f . elems

-- 容易实现encode
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 6 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard = "101"
centerGuard = "01010"

{-
解决此问题前我们先细分如何处理
1.转换图片到某个格式
2.猜测图片所有可能的编码
3.列出可能正确的编码
-}
{-
转换到netpbm格式，与PNM很像，
不同的地方 开头为P6;二进制部分由3字节表示，分别代表RGB值
-}
type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)
type Pixmap = Array (Int, Int) RGB

parseRawPPM :: Parse Pixmap
parseRawPPM =
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P6") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxValue ->
    assert (maxValue == 255) "max value out of spec" ==>&
    parseByte ==>&
    parseTimes (width * height) parseRGB ==> \pxs ->
    --identity (listArray ((0,0),(width-1,height-1)) pxs)
    identity (listArray ((0,0),(height-1,width-1)) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte ==> \r ->
           parseByte ==> \g ->
           parseByte ==> \b ->
           identity (r,g,b)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x:) <$> parseTimes (n-1) p

{-
将彩色图片转换为灰度图,
-}
luminance :: (Pixel, Pixel, Pixel) -> Pixel
--luminance (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
luminance (r,g,b) = round (r' * 0.1 + g' * 0.45 + b' * 0.45)
    where
        r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b


type Greymap = Array (Int,Int) Pixel

pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance

data Bit = Zero | One
           deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
    where
        binary i
            | i < pivot  = Zero
            | otherwise  = One
        pivot    = round $ least + (greatest - least) * n
        least    = fromIntegral $ choose (<) a
        greatest = fromIntegral $ choose (>) a
        choose f = foldA1 $ \x y -> if f x y then x else y

{-
分割像素,取得每段的长度
-}
type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
    where rle xs = (length xs, head xs)

runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

{-
比例缩放像素，对比编码表，找到前3个候选编码
-}
type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
    where divide d = fromIntegral d / divisor
          divisor = fromIntegral (sum xs)

type ScoreTable = [[Score]]
type Digit = Word8

asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores
    where
        scores = zip [distance d (scaleToOne ps) | d <- srl] digits
        digits = [0..9]

{-
选取候选匹配的同时记录奇偶编码,用来计算校验值
-}
data Parity a
    = Even a
    | Odd a
    | None a
    deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
    fmap = parityMap

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g x y = g x `f` g y

compareWithoutParity = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
              ((map Odd (bestScores leftOddSRL ps)) ++
               (map Even (bestScores leftEvenSRL ps)))

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs in h : chunkWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One):_) = []
candidateDigits rle
    -- 59 = 3 * 2 + 5 * 1 + (6 + 6) * 4
    | length rle < 59 = []
    | any null match = []
    | otherwise = map (map (fmap snd)) match
    where
        match = map bestLeft left ++ map bestRight right
        left = chunksOf 4 . take 24 . drop 3 $ runLengths
        right = chunksOf 4 . take 24 . drop 32 $ runLengths
        runLengths = map fst rle

input = zip (runLengths $ encodeEAN13 "978013211467") (cycle [Zero, One])

{-
因为可能性太多，我们先通过计算校验位来排除掉部分候选值
-}

-- 校验位和满足校验位的序列
type Map a = M.Map Digit [a]

type DigitMap = Map Digit
type ParityMap = Map (Parity Digit)

updateMap :: Parity Digit       -- ^ new digit
          -> Digit              -- ^ existing key
          -> [Parity Digit]     -- ^ existing digit sequence
          -> ParityMap          -- ^ map to update
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit =
    --new `M.union` M.foldWithKey (updateMap digit) M.empty old
    new `M.union` M.foldrWithKey (updateMap digit) M.empty old

incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old digits = foldl' (useDigit old) M.empty digits

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 [])
            . mapEveryOther (map (fmap (*3)))

firstDigit :: [Parity a] -> Digit
firstDigit = snd . head . bestScores paritySRL . runLengths
           . map parityBit . take 6
    where
        parityBit (Even _) = Zero
        parityBit (Odd _) = One

addFirstDigit :: ParityMap -> DigitMap
--addFirstDigit = M.foldWithKey updateFirst M.empty
addFirstDigit = M.foldrWithKey updateFirst M.empty

updateFirst :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
updateFirst key seq = insertMap key digit (digit:renormalize qes)
    where
        renormalize = mapEveryOther (`div` 3) . map fromParity
        digit = firstDigit qes
        qes = reverse seq

buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -) . addFirstDigit . finalDigits

solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where
        checkDigits = map fromParity (last xs)
        m = buildMap (init xs)
        addCheckDigit m k = (++[k]) <$> M.lookup k m

withRow :: Int -> Pixmap -> (RunLength Bit -> a) -> a
withRow n greymap f = f . runLength . elems $ posterized
    where
        posterized = threshold 0.4 . fmap luminance . row n $ greymap

--row :: (Ix a, Ix b) => b -> Array (a,b) c -> Array a c
--row j a = ixmap (l,u) project a
--    where
--        project i = (i,j)
--        ((l,_), (u,_)) = bounds a

row :: (Ix a, Ix b) => a -> Array (a, b) c -> Array b c
row j a = ixmap (l, u) project a
    where
        project i = (j, i)
        ((_, l), (_, u)) = bounds a

findMatch :: [(Run, Bit)] -> Maybe [[Digit]]
findMatch = listToMaybe
          . filter (not . null)
          . map (solve . candidateDigits)
          . tails

--findEAN13 :: Pixmap -> Maybe [Digit]
--findEAN13 pixmap = withRow center pixmap (fmap head . findMatch)
--    where
--        (_, (maxX, _)) = bounds pixmap
--        center = (maxX + 1) `div` 2

findEAN13' :: Pixmap -> [(Run,[Digit])]
findEAN13' pixmap = map rle . sortBy ((flip compare) `on` length) . group . sort $ searchRowAt maxX
    where
        (_, (maxX, _)) = bounds pixmap
        searchRowAt 0 = []
        searchRowAt n = case withRow n pixmap (fmap head . findMatch) of
            Nothing -> searchRowAt (n-1)
            Just x -> x : searchRowAt (n-1)
        rle xs = (length xs, head xs)

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \arg -> do
        e <- parse parseRawPPM <$> L.readFile arg
        case e of
            Left err ->     print $ "error: " ++ err
            Right pixmap -> print $ findEAN13' pixmap
