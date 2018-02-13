{-# LANGUAGE ScopedTypeVariables #-}

module ControlledVisit where

import Control.Monad (filterM, liftM, forM)
import System.Directory
    ( Permissions(..)
    , getModificationTime
    , getPermissions
    , getDirectoryContents
    )
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import Data.List (sortBy)
import Data.Char (toLower)

data Info = Info
    { infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_ :: SomeException) -> return Nothing) (Just `liftM` act)

pathTraverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
pathTraverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then pathTraverse order (infoPath info)
            else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

exercise1 = pathTraverse (sortBy $ flip compare)

exercise2 = pathTraverse (\(x:xs) -> xs ++ [x])

{-
对于pathTraverse，我们提供了控制目录遍历过程的方式，
但是我们依然没有办法控制单个目录中的遍历，
会返回整个目录中的文件后才能进行后续操作,
如果一个目录中有很多文件，我们想从其中筛选出前n个，
就会有很大的浪费
将遍历过程延迟计算的方式就是参考列表处理使用fold
-}
