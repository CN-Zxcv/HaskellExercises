
{-
与LANGUAGE 的区别？？
-}
--{-# OPTIONS_GHC -XDatatypeContexts #-}
{-# OPTIONS_GHC -XTypeSynonymInstances #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module RunProcessSimple where

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import Text.Regex.Posix
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import Control.Exception
import System.Posix.Env (getEnv)
import System.Directory
    ( getCurrentDirectory
    , setCurrentDirectory
    , createDirectory
    )

-- 命令
type SysCommand = (String, [String])

-- 返回结果
data CommandResult = CommandResult
    { cmdOutput :: IO String
    , getExistStatus :: IO ProcessStatus
    }

{-
MVar
-}
-- 必须在新的子进程中关闭的文件描述符列表
type CloseFDs = MVar [Fd]

-- 运行命令的接口
class CommandLike a where
    invoke :: a -> CloseFDs -> String -> IO CommandResult

-- 执行系统命令
instance CommandLike SysCommand where
    invoke (cmd, args) closefds input = do
        {-
        创建两个管道，用来把我们执行的命令包在中间,
        stdinwrite -> (cmd stdinread) -> stdoutwrite .. stdoutread
        -}
        (stdinread, stdinwrite) <- createPipe
        (stdoutread, stdoutwrite) <- createPipe

        {-
        pipe在写端写入文件结束符后才会将结果传递给读端，
        所以pipe关闭写句柄的时候才会将输入往下传递

        主动写入结束符是什么效果??

        因为执行forkProcess会复制主进程的除了线程的所有东西
        所以文件句柄也被复制了，会导致句柄无法准时释放
        所以在子进程中专门关闭一下
        -}
        {-
        这里通过复制句柄并关闭不同的端口实现进程间的管道连接
        -}
        addCloseFDs closefds [stdinwrite, stdoutread]

        childPID <- withMVar closefds (\fds ->
            forkProcess (child fds stdinread stdoutwrite)
            )

        closeFd stdinread
        closeFd stdoutwrite

        -- 主进程向子进程发命令
        stdinhdl <- fdToHandle stdinwrite
        {-
        使用forkIO将写入管道的过程放到另外的进程进行
        进一部可以通过HSH库将进程的管道不走Haskell直接连接,速度会更快
        -}
        forkIO $ do
            hPutStr stdinhdl input
            hClose stdinhdl

        -- 接收执行结果
        stdouthdl <- fdToHandle stdoutread

        -- 返回结果的处理函数
        let waitfunc = do
            status <- getProcessStatus True False childPID
            case status of
                Nothing -> fail "Error: Nothing from getExistStatus"
                Just ps -> do
                    removeCloseFDs closefds [stdinwrite, stdoutread]
                    return ps

        return $ CommandResult
            { cmdOutput = hGetContents stdouthdl
            , getExistStatus = waitfunc
            }
        where
            child closefds stdinread stdoutwrite = do

                -- 将传进来的句柄设置成本进程的标准输入输出
                dupTo stdinread stdInput
                dupTo stdoutwrite stdOutput

                closeFd stdinread
                closeFd stdoutwrite

                -- 关掉不需要的被动复制的句柄
                mapM_ (\fd -> catch (closeFd fd) (\(SomeException e) -> return ())) closefds

                -- 执行
                executeFile cmd True args Nothing

-- 向fork关闭句柄列表加句柄
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds =
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- 从列表删除句柄
removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds removethem =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)
    where
        procfdlist fdlist [] = fdlist
        procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

        removefd [] _ = []
        removefd (x:xs) fd
            | fd == x = xs
            | otherwise = x : removefd xs fd

-- 定义管道
data PipeCommand src dest = PipeCommand src dest

(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

instance (CommandLike a, CommandLike b) => CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dest) closefds input = do
        res1 <- invoke src closefds input
        output1 <- cmdOutput res1
        res2 <- invoke dest closefds output1
        return $ CommandResult (cmdOutput res2) (getEC res1 res2)

-- 合并管道两端的结果
getEC :: CommandResult -> CommandResult -> IO ProcessStatus
getEC src dest = do
    sec <- getExistStatus src
    dec <- getExistStatus dest
    case sec of
        Exited ExitSuccess -> return dec
        x -> return x

runIO :: CommandLike a => a -> IO ()
runIO cmd = do
    closefds <- newMVar []

    res <- invoke cmd closefds []

    output <- cmdOutput res
    putStr output

    ec <- getExistStatus res
    case ec of
        Exited ExitSuccess -> return ()
        x -> fail $ "Exited: " ++ show x

{-
$> runIO $ ("ls", ["/usr"]) -|- ("grep", "^l")
-}

-- 直接字符串解析成shell命令
instance CommandLike String where
    invoke cmd closefds input = do
        esh <- getEnv "SHELL"
        let sh = case esh of
                Nothing -> "/bin/sh"
                Just x -> x
        invoke (sh, ["-c", cmd]) closefds input
{-
$> runIO $ "ls"
-}

-- 支持将管道内容传递给Haskell函数
instance CommandLike (String -> IO String) where
    invoke func _ input =
        return $ CommandResult (func input) (return (Exited ExitSuccess))

-- 支持haskell纯函数
instance CommandLike (String -> String) where
    invoke func = invoke iofunc
        where
            iofunc :: String -> IO String
            iofunc = return . func

instance CommandLike ([String] -> IO [String]) where
    invoke func _ input =
        return $ CommandResult linedfunc (return (Exited ExitSuccess))
        where
            linedfunc = func (lines input) >>= return . unlines

instance CommandLike ([String] -> [String]) where
    invoke func = invoke (unlines . func . lines)

-- 多种返回类型

class RunResult a where
    run :: (CommandLike b) => b -> a

setUpCommand :: CommandLike a => a -> IO CommandResult
setUpCommand cmd = do
    closefds <- newMVar []
    invoke cmd closefds []

instance RunResult (IO ()) where
    run cmd = run cmd >>= checkResult

instance RunResult (IO ProcessStatus) where
    run cmd = do
        res <- setUpCommand cmd
        output <- cmdOutput res
        putStr output

        getExistStatus res

instance RunResult (IO Int) where
    run cmd = do
        rc <- run cmd
        case rc of
            Exited (ExitSuccess) -> return 0
            Exited (ExitFailure x) -> return x
            (Terminated x) -> return (128 + (fromIntegral x))
            Stopped x -> return (128 + (fromIntegral x))

instance RunResult (IO Bool) where
    run cmd = do
        rc <- run cmd
        return ((rc :: Int) == 0)

instance RunResult (IO [String]) where
    run cmd = do
        r <- run cmd
        return (lines r)

instance RunResult (IO String) where
    run cmd = do
        res <- setUpCommand cmd
        output <- cmdOutput res

        evaluate (length output)

        ec <- getExistStatus res
        checkResult ec
        return output

checkResult :: ProcessStatus -> IO ()
checkResult ps = do
    case ps of
        Exited (ExitSuccess) -> return ()
        x -> fail (show x)

runIO' :: CommandLike a => a -> IO ()
runIO' = run

cd :: FilePath -> IO ()
cd = setCurrentDirectory

echo :: String -> String -> String
echo inp _ = inp

--grep :: String -> [String] -> [String]
--grep pat = filter (ismatch regex)
--    where
--        regex = mkRegex pat
--        ismatch r inp = case matchRegex r inp of
--            Nothing -> False
--            Just _ -> True
