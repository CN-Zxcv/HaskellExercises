

-- pcre *pcre_compile(const char *pattern,
--                    int options,
--                    const char **errptr,
--                    int *erroffset,
--                    const unsigned char *tableptr);


{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

{-
pcre *
由于该 C 语言类型被抽象地处理，我们可以为该数据赋值给任何堆分配的 Haskell 类型，
只要其上没有或几乎没有操作即可。
表示未知外部数据的惯用简单类型是指向 () 类型的指针
-}
-- type PCRE = ()
{-
更暴力的是使用空类型，即未定义的类型，如果haskell解引用PCRE，必然无法通过编译
保证了haskell端绝不会解引用PCRE
-}
data PCRE

{-
safe unsafe

safe
foreign import 的时候默认是safe的，
保证 Haskell 系统能够在 C 语言中安全地调用,代码可以回调回Haskell
会慢一些

unsafe
调用 C 语言代码不能回调到 Haskell 中
调用的开销要少得多，

-}

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile :: CString
                   -> PCREOption
                   -> Ptr CString
                   -> Ptr CInt
                   -> Ptr Word8
                   -> IO (Ptr PCRE)
