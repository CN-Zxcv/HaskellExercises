{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}

module Regex where

import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString (ByteString, useAsCString, empty)
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Char8 (pack, unpack)

{-
-XCPP
会对此文件调用C的语言预处理器，翻译其中
-- #{enum}
-- #const
等特殊标记内部的C代码
-}

{-# LINE 22 "Regex.hsc" #-}

newtype PCREOption = PCREOption {unPCREOption :: CInt}
    deriving (Eq, Show)

{-
** 使用 hsc2hs 绑定 C 常量 到 Haskell
通过hsc生成hs，会将c bind 相关的代码模板翻译为实际代码

$> hsc2hs Regex.hsc

一种方式是通过#const

-- #define PCRE_CASELESS           0x00000001
-- #define PCRE_MULTILINE          0x00000002
-- #define PCRE_DOTALL             0x00000004
-- #define PCRE_EXTENDED           0x00000008

caseless :: PCREOption
caseless = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall         :: PCREOption
dotall         = PCREOption #const PCRE_DOTALL

我们也有更简洁一点的 enum宏
-}
caseless  :: PCREOption
caseless  = PCREOption 1
dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
dotall  :: PCREOption
dotall  = PCREOption 4

{-# LINE 55 "Regex.hsc" #-}


combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

{-
-- pcre *pcre_compile(const char *pattern,
--                    int options,
--                    const char **errptr,
--                    int *erroffset,
--                    const unsigned char *tableptr);
-}

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

data Regex = Regex !(ForeignPtr PCRE) !ByteString
    deriving (Eq, Ord, Show)

{-
unsafePerformIO :: IO a -> a
强制转换IO到无副作用类型,函数正确性由调用方保证

useAsCString :: ByteString -> (CString -> IO a) -> IO a
将ByteString中的值拷贝到以\0结尾的CString中,
然后用这个CString执行函数
也有一个不安全的零拷贝变体，它假定 ByteString 已经是空结尾

alloca :: Storable a => (Ptr a -> IO b) -> IO b
分配内存
alloca $ \stringptr -> do
    ... call some Ptr CString function
    peek stringptr

-}

{-
ForeignPtr
    newForeignPtr :: finalizerPtr a -> Ptr -> IO (ForeignPtr a)
        离开作用域时(其他地方不再使用)调用finalizerPtr回收Ptr
    withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
        ForeignPtr 中提取出Ptr进行使用
        使用到Ptr的代码都必须在withForeignPtr里面，
        在作用域外则不能保证Ptr在什么时候被回收了
-}

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
    useAsCString str $ \pattern -> do
        alloca $ \errptr -> do
        alloca $ \erroffset -> do
            pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
            if pcre_ptr == nullPtr
                then do
                    err <- peekCString =<< peek errptr
                    return (Left err)
                else do
                    reg <- newForeignPtr finalizerFree pcre_ptr
                    return (Right (Regex reg str))

{-
$>ghci PCRE-compile.hs -lpcre
-}

{-
命名子模式(named subpattern)
非命名子模式(numbering subpattern)

PCRE_INFO_CAPTURECOUNT: 得到的是所有子模式的个数,包含上述的两种子模式个数;
PCRE_INFO_NAMECOUNT: 得到的是命名子模式的个数,不包括非命名子模式的个数;
PCRE_INFO_NAMETABLE: 对于只有命名子模式的情况,例如文档描述的一个例子:
-}
newtype PCREInfo = PCREInfo {unPCREInfo :: CInt}

info_capturecount  :: PCREInfo
info_capturecount  = PCREInfo 2

{-# LINE 164 "Regex.hsc" #-}

data PCREExtra

{-
PCRE_EXP_DECL int  pcre_fullinfo(const pcre *, const pcre_extra *, int,
                void *);

-}
foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt


capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do
        c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
        return . fromIntegral =<< peek (n_ptr :: Ptr CInt)

newtype PCREExecOption = PCREExecOption {unPCREExecOption :: CInt}

pcre_exec_anchored  :: PCREExecOption
pcre_exec_anchored  = PCREExecOption 16
pcre_exec_notbol  :: PCREExecOption
pcre_exec_notbol  = PCREExecOption 128
pcre_exec_noteol  :: PCREExecOption
pcre_exec_noteol  = PCREExecOption 256
pcre_exec_notempty  :: PCREExecOption
pcre_exec_notempty  = PCREExecOption 1024

{-# LINE 194 "Regex.hsc" #-}

combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0

{-
-- int pcre_exec(const pcre *code,
--               const pcre_extra *extra,
--               const char *subject,
--               int length,
--               int startoffset,
--               int options,
--               int *ovector,
--               int ovecsize);

-}
foreign import ccall "pcre.h pcre_exec"
    c_pcre_exec     :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> Ptr Word8
                    -> CInt
                    -> CInt
                    -> PCREExecOption
                    -> Ptr CInt
                    -> CInt
                    -> IO CInt
{-
对输入数据使用严格的 ByteString，我们可以在无需复制的情况下提取已匹配子串，使接口效率更高
-}
match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
    withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr
    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)
    allocaBytes ovec_bytes $ \ovec -> do
        let (str_fp, off, len) = toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                pcre_ptr
                nullPtr
                (cstr `plusPtr` off)
                (fromIntegral len)
                0
                (combineExecOptions os)
                ovec
                (fromIntegral ovec_size)
            if r < 0
                then return Nothing
                else
                    let loop n o acc =
                            if n == r
                                then return (Just (reverse acc))
                                else do
                                    i <- peekElemOff ovec o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    loop (n+1) (o+2) (s : acc)
                    in loop 0 0 []
            where
                substring :: CInt -> CInt -> ByteString -> ByteString
                substring x y _ | x == y = empty
                substring a b s = end
                    where
                        start = unsafeDrop (fromIntegral a) s
                        end   = unsafeTake (fromIntegral (b-a)) start

testMatch :: String -> String -> Maybe [String]
testMatch pat str = (fmap . fmap) unpack r
    where
        (Right reg) = compile (pack pat) []
        r = match reg (pack str) []
