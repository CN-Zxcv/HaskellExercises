
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

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
