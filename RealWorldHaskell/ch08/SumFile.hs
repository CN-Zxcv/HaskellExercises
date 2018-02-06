

{-
String 对于大文本的处理效率很差，
因为 String = [Char],
对于每个值都是单独分配内存的，并且每个字符都有payload
这种情况下 Text ByteString 通常是更好的选择
-}

main = do
    contents <- getContents
    print (sumFile contents)
  where sumFile = sum . map read . words
