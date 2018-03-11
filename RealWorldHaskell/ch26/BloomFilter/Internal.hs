
{-
控制名称可见性，不导出模块
-}

module BloomFilter.Internal (
      Bloom(..)
    , MutBloom(..)
    ) where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

data Bloom a = B {
      blmHash :: (a -> [Word32])
    , blmArray :: UArray Word32 Bool
    }

{-
UArray
    常规的Hask类型都是提升的，也就是说可以包含bottom或者undefined等特殊值
    这种提升增加了间接层,带来了很多方便，但是会有一定的性能损失,
    在表示大量数据和位的时候使用未提升的类型效果更好

ST Monad (State Transformer Monad)

    通过 可变引用（mutable references） 可以构建出一种数据结构，
    这种数据结构允许用户像命令式语言一样随时对其进行修改
    在未找到合适的算法来代替某些命令式语言的算法时，
    我们可以通过可变数据的方式来实现相同的功能

        解冻一个可变数组得到可变数组，然后原地修改数组，最后重新冻结得到新的不可变数组

    也遵循引用透明
        可变数据在ST Monad内部使用,但是不会逃逸到外部
        在ST Monad内部不能读写文件，创建全局变量，或者创建线程

-}

data MutBloom s a = MB {
      mutHash :: (a -> [Word32])
    , mutArray :: STUArray s Word32 Bool
    }
