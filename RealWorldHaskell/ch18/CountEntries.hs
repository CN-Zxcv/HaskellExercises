{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CountEntries
  ( listDirectory
  , countEntriesTrad
  ) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, forM_, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)

import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace


listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
    contents <- listDirectory path
    rest <- forM contents $ \name -> do
        let newName = path </> name
        isDir <- doesDirectoryExist newName
        if isDir
          then countEntriesTrad newName
          else return []
    return $ (path, length contents) : concat rest

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntries newName

{-
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

class Monad m => MonadReader r m | m -> r where
    ask ..
    local ..
    reader ..

instance Monad m => MonadReader r (ReaderT r m) where
    ...

type Reader r = ReaderT * r Identity

Monad在库中的定义通常如上,
接口MonadReader
然后MonadTransform
Reader等的是MonadTransform的一个实例

但是IO是个特例
    IOT是无法实现的
-}

{-
多个monad叠加组合成更强大的Monad时, IO 因为没有IOT所以必须在最底层
-}

data AppConfig = AppConfig
    { cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState
    { stDeepestReached :: Int
    } deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)
{-
type App2 = ReaderT AppConfig (StateT AppState IO) a
如果这样些会导致再往上叠加的时候 App2 会无法通过编译
type T1 = WriterT [String] App
type T2 a = WriterT [String] App2 a
因为App2 需要提供一个参数, 而上面的WriterT就没有提供参数
-}

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in  runStateT (runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
    contents <- liftIO . listDirectory $ path
    cfg <- ask
    rest <- forM contents $ \name -> do
        let newPath = path </> name
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir && curDepth < cfgMaxDepth cfg
            then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                    put st {stDeepestReached = newDepth}
                constrainedCount newDepth newPath
            else return []
    return $ (path, length contents) : concat rest

{-
newtype 封装实现
-}
newtype MyApp a = MyA
    { runA :: ReaderT AppConfig (StateT AppState IO) a
    } deriving (Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in  runStateT (runReaderT (runA k) config) state


{-
lift
-}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
    unwrapped <- runMaybeT x
    case unwrapped of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)
returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    (>>=) = bindMT
    fail = failM

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put k = lift (put k)
