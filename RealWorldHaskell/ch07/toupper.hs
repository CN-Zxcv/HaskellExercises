
import System.IO
import Data.Char (toUpper)

main1 :: IO ()
main1 = do
    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    mainloop inh outh
    hClose inh
    hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
    ineof <- hIsEOF inh
    if ineof
        then return ()
        else do
            inpStr <- hGetLine inh
            hPutStrLn outh (map toUpper inpStr)
            mainloop inh outh

main2 :: IO ()
main2 = do
    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    inpStr <- hGetContents inh

    -- 这里以后不再使用inpStr, ghc会找机会回收掉已使用的部分，
    -- 这样访问大文件时就不会内存就不会爆炸,
    -- 如果我们在后续的地方引用了inpStr, 这里就不会被回收，
    -- ***memory is only freed after its last use
    let result = map toUpper inpStr

    hPutStr outh result

    -- hClose 在这里不是必要的, 当使用hGetContents 时, Handle将处于半关闭状态
    -- 当以下发生时，文件被真的关闭
    -- * 应用hClose
    -- * I/O 发生异常
    -- * 所有内容都被读取完毕
    hClose inh
    hClose outh

main3 :: IO ()
main3 = do
    inpStr <- readFile "input.txt"
    writeFile "output.txt" (map toUpper inpStr)
