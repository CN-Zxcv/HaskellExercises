
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
--import System.Time (ClockTime(..))
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
--import Control.Exception (bracket, handle)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)


import RecursiveContents (getRecursiveContents)

type Predicate
    = FilePath -- path to directory
    -> Permissions -- permissions
    -> Maybe Integer    -- file size
    -> UTCTime -- last modified
    -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: SomeException) -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(_ :: SomeException) -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where
        check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return $ p name perms size modified

{-
按照上面的定义，会导致参数特别长，而且很多情况下只使用到了一部分参数，多余的参数造成了混淆
下面的将这部分写的更简洁的方式是写一个DSL，
-}
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

-- 首先是需要定义返回其中一个参数的函数，以便上层函数使用
-- 比如如下一个
-- pathP path _ _ _ = path
-- 可以看到这个的类型是
-- (FilePath -> Permissions -> Maybe Integer -> UTCTime -> FilePath)
-- 与我们的check函数的类型差别就在返回值上
-- DSL在这种情况下主要就是将返回值多态化，然后通过组合函数返回想要的结果,
-- 以此提供比直接定义check更高的抽象,

type InfoP a
    = FilePath
    -> Permissions
    -> Maybe Integer
    -> UTCTime
    -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

-- 我们定义一个简单的(==)抽象 (sizeP `equalP` 1024)
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

-- 当然我们可以基于柯里化不使用匿名函数，直接定义如下
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

-- 有了equal，我们还会有 (<) (>) 等二元操作
-- 所以我们提供一层抽象,(liftP) 接受一个普通二元函数来合成equalP等函数
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

-- 同理3元函数会有(liftP2)
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

-- liftP可以用liftP2实现
constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' q f k w x y z = f w x y z `q` constP k w x y z

--
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

-- 有了以上后 我们的myTest可以显得更自然(没有了冗余的一堆无用的参数)
myTest2 =
    (liftPath takeExtension `equalP` ".cpp")
    `andP` (sizeP `greaterP` 131072)

-- 如果我们给equalP等定义操作符,或许看起来更好
-- (模块中非常常用的核心操作或者与已有操作符相似的操作可以定义成操作符，
-- 但是很多情况下定义操作符会导致可读性降低)
(==?) = equalP
(&&?) = andP
(>?) = greaterP

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

-- 在未定义操作符优先级的时候，都默认为9(最低)
-- 我们可以通过定义优先级来去掉括号
-- 当然这样可读性并不一定能提升
infix 4 ==?
infix 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
