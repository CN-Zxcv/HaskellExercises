{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
hide IO monad
定义只作用与本地文件的IO
-}

module HandleIO (
      HandleIO
    , Handle
    , IOMode (..)
    , runHandleIO
    , openFile
    , hClose
    , hPutStrLn
    ) where

import System.IO (Handle, IOMode (..))
import qualified System.IO

import Control.Monad.Trans (MonadIO(..))
import System.Directory (removeFile)

-- newtype 隐藏 IO
newtype HandleIO a = HandleIO {runHandleIO :: IO a}
    deriving (Monad)

-- 定义函数隐藏IO的函数
openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h

{-
runHandleIO (safeHello "hello.txt")
-}


instance MonadIO HandleIO where
    liftIO = HandleIO

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
    safeHello path
    liftIO (removeFile path)

{-
关于如何隐藏实现

实现一般涉及 Data Function TypeClass

隐藏实现的方式就是定义一套与实现同构的API

** Data 通过 newtype 隐藏

newtype HandleIO a = HandleIO {runHandleIO :: IO a}

** funcition 封装一层

openFile :: FilePath -> IOMode -> Handle
openFile' :: FilePath -> IOMode -> HandleIO Handle

通常为了方便定义openFile等函数
一般会有一个typeclass方便将类型提升到另外的类型
方便用户扩展，
也保留了某些情况下封装不完善时用户需要调用底层实现的可能

我们这里 IO就有MonadIO typeclass

class Monad m => MonadIO m where
    liftIO ::

instance MonadIO IO where
    ...

instance MonadIO HandleIO where
    liftIO = HandleIO

我们也可以为当前的类型保留一个扩展接口
class MonadHandleIO m where
    ...

removeFile :: FilePath -> IO ()
liftIO (removeFile path) :: HandleIO ()


** 进一部使用typeclass定义我们需要导出的函数集合
class Monad m => MonadHandle h m | m -> h where
    openFile :: FilePath IOMode m h
    hClose :: h -> m ()
    ...

这里涉及的class定义相关在SupplyClass.hs里面

然后对封装的类型做一个实现
instance MonadHandle System.IO.Handle IO where
    ...

在外部,除了类型约束，外部几乎就不再关心实现，
内部做出的修改只要不涉及API变化，就没有任何影响

safeHello :: MonadHandle h m => FilePath -> m ()

-}
