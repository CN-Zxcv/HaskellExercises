
module NiceFork (
      ThreadManager
    , newManager
    , forkManaged
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Exception (SomeException, try)
import qualified Data.Map as M
import Data.Maybe

import Debug.Trace

data ThreadStatus =
      Running
    | Finished
    | Threw SomeException
    deriving (Show)

{-
Haskell 主线程结束后会强制结束掉其他线程，
所以有时候需要保证主线程在其他线程结束前不退出
-}

{-
我们需要个线程间同步的数据来管理线程状态,
所以整体数据是放在MVar中的
每个线程的状态也是MVar
-}

newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)

-- | 创建一个新的线程管理器
newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

{-
try :: Exception e => IO a -> IO (Either e a)
either (a -> c) -> (b -> c) -> Either a b -> c
-}
-- | 创建一个被管理的线程
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body = modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
        result <- try body
        putStrLn $ "forkManaged.result: " ++ show result
        putMVar state (either Threw (const Finished) result)
    return (M.insert tid state m, tid)

-- | 返回线程状态
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid = modifyMVar mgr $ \m ->
    case M.lookup tid m of
        Nothing -> trace ("not found tid: " ++ show tid) $ return (m, Nothing)
        Just st -> tryTakeMVar st >>= \mst ->
            case mst of
                Nothing -> return (m, Just Running)
                Just sth -> return (M.delete tid m, Just sth)

-- | 阻塞，直到特定线程被线程管理器终结
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
    maybeDone <- modifyMVar mgr $ \m ->
        return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
            (Nothing, _) -> (m, Nothing)
            (done, m') -> (m', done)
    case maybeDone of
        Nothing -> return Nothing
        Just st -> Just `fmap` takeMVar st
{-
join :: m (m a) -> m a
-}
waitFor2 :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor2 (Mgr mgr) tid = do
    join . modifyMVar mgr $ \m ->
        return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
            (Nothing, _) -> (m, return Nothing)
            (Just st, m') -> (m', Just `fmap` takeMVar st)

-- | 阻塞，直到所有线程被线程管理器终结
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where
        elems m = return (M.empty, M.elems m)

test = do
    let calc = do {calc ; return ()}
    manager <- newManager
    tid <- forkManaged manager calc
    putStrLn $ "tid is: " ++ show tid
    ans <- waitFor manager tid
    putStrLn $ show ans


{-
MVar 和 Chan 是非严格的，如果存入的是表达式，那么在进行获取的时候才进行求值
如果表达式是个很耗时的操作，那将会阻塞主线程
所以提供了MVar和Chan的严格版本 strict-concurrency
-}
