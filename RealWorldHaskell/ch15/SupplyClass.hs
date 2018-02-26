{-# LANGUAGE
      MultiParamTypeClasses
    , FunctionalDependencies
    , FlexibleInstances
    #-}


{-
通过间层隐藏接口

-}


{-
-XMultiParamTypeClasses
描述类型之间的关系

class Fun a b where
    fun :: a -> b

instance Fun Int Int where
    fun = undefined

instance Fun Int Char where
    fun = undefined

a = fun (1::Int) :: Int

但是如果将其中某个类型作为参数返回时，会导致返回的类型是多态的，
如果调用方也无法确定返回值的类型， 就无法确定调用了那个fun,
我们必须要手动加类型描述

-XFunctionalDependencies
这种情况下，有时候我们就对返回类型做出限定
| a b c -> d
class Fun a b | a -> b where
当a类型确定时，b类型也确定了，也就是b类型依赖a的类型，
对于同样的类型a，只能有一个b类型与之对应

我们就不能同时定义下面的两个instance,编译器将对此做检查,只能定义一个
instance Fun Int Int
instance Fun Int Char

-XFlexibleInstances
允许instance head 多次引用变量
instance [a] a where

但是不用的话
instance [a] [a] where
这样也是可以的

只是解除顶级限制 ??

-}

module SupplyClass
    ( MonadSupply(..)
    , S.Supply
    , S.runSupply
    ) where

import qualified Supply as S

class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
    next = S.next

showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
    a <- next
    b <- next
    return $ "a: " ++ show a ++ ", b: " ++ show b

{-
关于如何隐藏实现

实现一般涉及 Data Function TypeClass

隐藏实现的方式就是定义一套与实现同构的API

Data 通过 newtype 隐藏

newtype HandleIO a = HandleIO {runHandleIO :: IO a}

funcition 封装一层

openFile :: FilePath -> IOMode -> Handle
openFile' :: FilePath -> IOMode -> HandleIO Handle

-}
