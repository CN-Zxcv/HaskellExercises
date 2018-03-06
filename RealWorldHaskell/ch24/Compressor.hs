
import Control.Concurrent (forkIO)
import Control.Exception (handle, SomeException)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import System.Console.Readline (readline)

import Codec.Compression.GZip (compress)

{-
forkIO :: 创建线程.执行完毕后关闭
-}

main = do
    maybeLine <- readline "Enter a file to compress"
    case maybeLine of
        Nothing -> return ()
        Just "" -> return ()
        Just name -> do
            handle (print :: (SomeException -> IO ())) $ do
                content <- L.readFile name
                forkIO (compressFile name content)
                return ()
            main
    where
        compressFile path = L.writeFile (path ++ ".gz") . compress
