
import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)

{-
simpleFind的问题
1.无法区分文件和目录
2.无法过滤不想遍历的目录
3.严格求值的，当在io monad中执行代码时，每一个动作都必须被执行才能得到结果,
  所以getRecursiveContents必须返回所有的目录后filter才会执行
  为啥getContents又是lazy的??
-}
