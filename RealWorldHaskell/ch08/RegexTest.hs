
import Text.Regex.Posix
import Data.ByteString.Char8

{-
Regex模块主要使用 (=~) [带匹配字符串] [模式]
可以使用String | ByteString 作为参数
-}

main :: IO ()
main = do
    -- Bool 类型表示是否有匹配
    print ("my left foot" =~ "foo" :: Bool)
    print ("your right hand" =~ "bar" :: Bool)
    print ("your right hand" =~ "(hand|foot)" :: Bool)

    -- Int 类型表示有多少个匹配
    print ("a star called henry" =~ "plant" :: Int)
    print ("abcdabcde" =~ "[aeiou]" :: Int)

    -- String 类型返回第一个成功匹配的子串
    print ("I, B. Ionsonii, uurit a lift`d batch" =~ "(uu|ii)" :: String)
    print ("hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" :: String)

    {-
     [String] 类型返回所有的匹配结果
    print ("I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: [String])
    print ("hi ludi, F. Baconis nati, tuiti orbi" =~ "Shakespeare" :: [String])

     **不能正确工作，要把类型换成 [[String]] 才行

     使用getAllTextMatches
    -}
    print (getAllTextMatches ("I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)") :: [String])

    {-
     (String, String, String)
        | Success -> (第一个匹配前的串，第一个匹配，第一个匹配后的串)
        | Failure -> (本字符串, "", "")
    -}
    let pat = "(foo[a-z]*bar|quux)"
    print ("before foodiebar after" =~ pat :: (String,String,String))

    {-
    (String, String, String, [String])
        | Success -> (第一个匹配前的串，第一个匹配，第一个匹配后的串, [所有匹配的子串])
        | Failure -> (本字符串, "", "", [])
    -}
    print ("before foodiebar after" =~ pat :: (String,String,String,[String]))

    {-
    (Int, Int)
        | Success -> (第一个匹配的位置，第一个匹配的长度)
        | Failure -> (-1, 0)
    -}
    print ("before foodiebar after" =~ pat :: (Int,Int))
    {-
    [(Int, Int)]
        | Success -> [(位置，长度)]
        | Failure -> []
    -}
    print (getAllMatches ("mondegreen" =~ pat) :: [(Int,Int)])

    -- 参数为 ByteString 时同样有效
    print (pack "foo" =~ "bar" :: Bool)
    print ("foo" =~ pack "bar" :: Bool)
    print (getAllMatches (pack "foo" =~ pack "o") :: [(Int, Int)])
    print (getAllTextMatches (pack "good food" =~ ".ood") :: [ByteString])

    -- To be Continued ...
