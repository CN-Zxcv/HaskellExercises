
module LineChunks (
      chunkedReadWith
    ) where

import Control.Exception (bracket, finally)
import Control.Monad (forM, forM_, liftM)
import Control.Parallel.Strategies (NFData, rdeepseq)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Conc (numCapabilities)
import System.IO
import Control.DeepSeq (rnf)

import System.Environment (getArgs)
import MapReduce (mapReduce)

{-
块的位置和大小
-}
data ChunkSpec = CS {
      chunkOffset :: Int64
    , chunkLength :: Int64
    } deriving (Eq, Show)

{-
对每个文件块应用处理函数
这里使用rnf来保证处理函数执行完毕了，否则可能因为懒计算的原因导致handle提前释放
如果处理函数内部使用了并行，rnf也可以保证并行执行完后才执行后续的逻辑
-}
withChunks :: (NFData a) =>
       (FilePath -> IO [ChunkSpec])
    -> ([LB.ByteString] -> a)
    -> FilePath
    -> IO a
withChunks chunkFunc process path = do
    (chunks, handles) <- chunkedRead chunkFunc path
    let r = process chunks
    (rnf r `seq` return r) `finally` mapM_ hClose handles

{-
按cpu核心数分割文件块并应用处理
-}
chunkedReadWith :: (NFData a) =>
       ([LB.ByteString] -> a)
    -> FilePath
    -> IO a
chunkedReadWith func path =
    withChunks (lineChunks (numCapabilities * 4)) func path

{-
按分块策略分割文件后读取并返回每个文件块的内容
-}
chunkedRead
    :: (FilePath -> IO [ChunkSpec])
    -> FilePath
    -> IO ([LB.ByteString], [Handle])
chunkedRead chunkFunc path = do
    chunks <- chunkFunc path
    liftM unzip . forM chunks $ \spec -> do
        h <- openFile path ReadMode
        hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
        chunk <- LB.take (chunkLength spec) `liftM` LB.hGetContents h
        return (chunk, h)

{-
将文件按换行符分成大致大小的几块
-}
lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
    bracket (openFile path ReadMode) hClose $ \h -> do
        totalSize <- fromIntegral `liftM` hFileSize h
        let chunkSize = totalSize `div` fromIntegral numChunks
            findChunks offset = do
                let newOffset = offset + chunkSize
                hSeek h AbsoluteSeek (fromIntegral newOffset)
                let findNewline off = do
                        eof <- hIsEOF h
                        if eof
                            then return [CS offset (totalSize - offset)]
                            else do
                                bytes <- LB.hGet h 4096
                                case LB.elemIndex '\n' bytes of
                                    Just n -> do
                                        chunks@(c:_) <- findChunks (off + n + 1)
                                        let coff = chunkOffset c
                                        return (CS offset (coff - offset):chunks)
                                    Nothing -> findNewline (off + LB.length bytes)
                findNewline newOffset
        findChunks 0

lineCount :: [LB.ByteString] -> Int64
lineCount = mapReduce rdeepseq (LB.count '\n') rdeepseq sum

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \path -> do
        numLines <- chunkedReadWith lineCount path
        putStrLn $ path ++ ": " ++ show numLines
