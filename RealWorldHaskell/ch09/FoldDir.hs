
{-
在定义fold前我们先描述我们的需求,
我们需要状态来决定遍历过程该做什么 完成 继续 停止 ...
-}

import ControlledVisit

import System.FilePath (takeExtension, (</>), takeFileName)
import Data.Char (toLower)

data Iterate acc
    = Done {unwrap :: acc} -- 停止遍历
    | Skip {unwrap :: acc} -- 跳过当前目录
    | Continue {unwrap :: acc} -- 继续遍历
    deriving (Show)

type Iterator acc = acc -> Info -> Iterate acc

-- 参考fold的定义实现foldTree
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initAcc path = do
    endAcc <- fold initAcc path
    return (unwrap endAcc)
    where
        fold acc subpath = getUsefulContents subpath >>= walk acc subpath
        walk acc path (name:names) = do
            let path' = path </> name
            info <- getInfo path'
            -- 与一般fold不同的就是带入了控制状态来控制fold的执行
            case iter acc info of
                done@(Done _) -> return done
                Skip acc' -> walk acc' path names
                Continue acc'
                    | isDirectory info -> do
                        next <- fold acc' path'
                        case next of
                            done@(Done _) -> return done
                            acc'' -> walk (unwrap acc'') path names
                    | otherwise -> walk acc' path names
        walk acc _ _ = return (Continue acc)

allIter :: Iterator [FilePath]
allIter paths info = Continue (infoPath info:paths)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
    | length paths == 3 = Done paths
    | isDirectory info && takeFileName path == ".svn" = Skip paths
    | extension `elem` [".jpg", ".png"] = Continue (path: paths)
    | otherwise = Continue paths
    where
        extension = map toLower (takeExtension path)
        path = infoPath info

countDirectories count info = Continue (if isDirectory info
    then count + 1
    else count)
