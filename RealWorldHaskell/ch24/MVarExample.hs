
import Control.Concurrent

{-
MVar
在多线程中
putMVar 将一个值放入，如果已有值，则线程会挂起。变空时,唤起一个写线程进行写入
takeMVar 从MVar取出值，如果为空，则挂起直到有值。有值时，唤起一个读线程进行读取

modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
a返回替换掉原有的a, b作为modify的返回值
从MVar取值并应用传入的函数,然后将值写回MVar,如果函数异常，则将原来的值写回去
线程间使用时比手动put/take安全的多
-}
communicate = do
    m <- newEmptyMVar
    n <- newEmptyMVar
    forkIO $ do
        v <- takeMVar m
        putStrLn ("received" ++ show v)
        putMVar n "send back"
    putStrLn "sending"
    putMVar m "wake up!"

    x <- takeMVar n
    putStrLn $ "received send back message: " ++ show x
