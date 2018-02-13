
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8)

{-
进一步我们还可以看到我们所有的处理几乎都是 (a, s) -> (a, s)
如果我们需要跟踪处理过程，我们就需要将类型改为 (a, s, t) -> (a, s, t)
为了扩展，我们定义一个state用来扩展，我们的处理变成 (a, st) -> (a, st)
-}
data ParseState = ParseState
    { string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

-- 如果我们需要返回过程中的错误，可以使用Either
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

-- 为了后续扩展，通常我们不向用户公开parse的详细实现,
-- 所以用newtype封装一层
newtype Parse a = Parse
    { runParse :: ParseState -> Either String (a, ParseState)
    }

-- 我们从最简单的什么都不做的parse开始
-- identity 拿到一个外部数据，什么都不做，用数据构造parse后直接返回
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

-- 定义我们需要的ByteString Parser
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = case runParse parser (ParseState initState 0) of
    Left err -> Left err
    Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState {offset = newOffset}

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
        case L.uncons (string initState) of
            Nothing ->
                bail "no more input"
            Just (byte, remainder) ->
                putState newState ==> \_ -> identity byte
                where
                    newState = initState
                        { string = remainder
                        , offset = newOffset
                        }
                    newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where
        chainedParser initState = case runParse firstParser initState of
            Left errMessage -> Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
