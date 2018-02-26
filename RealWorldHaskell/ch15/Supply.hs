{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
隐藏StateMonad防止用户改写State
方式是使用另外一个间层来隔离,所以新定义一套与State类似的结构

newtype Supply <--> State
runSupply <--> runState
next <--> get / set

这里我们更一般化的解决如何封装提供(唯一值/ID序列...)的问题

为何确定state是数组的 ??
作为ID集合,所以state类型应该是数组 ??

runState :: State s a -> s -> (a, s)
runSupply :: Supply s a -> [s] -> (a, [s])

State s a = s -> (a, s)
newtype Supply s a = S (State [s] a)

-}

--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--instance Monad (Supply s) where
--    s >>= m = S (unwrapS s >>= unwrapS . m)
--    return = S . return

module Supply (
      Supply
    , next
    , runSupply
    ) where

import Control.Monad.Trans.State
import Control.Applicative

--newtype Supply s a = S (State [s] a)
--    deriving (Functor, Applicative, Monad)
--
--runSupply :: Supply s a -> [s] -> (a, [s])
--runSupply (S m) xs = runState m xs

newtype Supply s a = S {runSupply :: State [s] a}
    deriving (Functor, Applicative, Monad)

next :: Supply s (Maybe s)
next = S $ do
    st <- get
    case st of
        [] -> return Nothing
        (x:xs) -> do
            put xs
            return (Just x)

-- runSupply next [1,2,3]

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

showTwo :: (Show s) => Supply s String
showTwo = do
    a <- next
    b <- next
    return $ "a: " ++ show a ++ ", b: " ++ show b

-- runSupply showTwo [1,2,3]
