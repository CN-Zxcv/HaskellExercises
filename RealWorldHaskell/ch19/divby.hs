{-# LANGUAGE FlexibleContexts #-}


--import Control.Monad.Error
import Control.Monad.Error

divBy :: Integral a => a -> [a] -> [a]
divBy n = map (n `div`)

{-
无法处理异常

ghci> divBy 50 [1,2,0,8,10]
[50,25,*** Exception: divide by zero
-}

divBy1 :: Integral a => a -> [a] -> Maybe [a]
divBy1 _ [] = Just []
divBy1 _ (0:_) = Nothing
divBy1 n (x:xs) = case divBy1 n xs of
    Nothing -> Nothing
    Just r -> Just ((n `div` x) : r)

{-
可以处理异常，但是丢失了惰性处理能力

ghci> divBy1 50 [1,2,5,8,10]
Just [50,25,10,6,5]

ghci> divBy1 50 [1,2,0,8,10]
Nothing

ghci> fmap (take 10) . divBy1 50 [1..]
*** Exception: stack overflow
-}


divBy2 :: Integral a => a -> [a] -> [Maybe a]
divBy2 n xs = map worker xs
    where
        worker 0 = Nothing
        worker x = Just (n `div` x)

{-
另外一种处理方式

ghci> divBy 50 [1,2,5,8,10]
[Just 50,Just 25,Just 10,Just 6,Just 5]

ghci> divBy 50 [1,2,0,8,10]
[Just 50,Just 25,Nothing,Just 6,Just 5]

ghci> take 5 (divBy 100 [1..])
[Just 100,Just 50,Just 33,Just 25,Just 20]
-}

divBy3 :: Integral a => a -> [a] -> Maybe [a]
divBy3 _ [] = return []
divBy3 _ (0:_) = fail "division by zero in divBy"
divBy3 n (x:xs) = do
    next <- divBy3 n xs
    return ((n `div` x) : next)
{-

我们使用 Maybe monad 来重写divBy

ghci> divBy 50 [1,2,5,8,10]
Just [50,25,10,6,5]
ghci> divBy 50 [1,2,0,8,10]
Nothing
ghci> divBy 100 [1..]
*** Exception: stack overflow
-}

{-
然后还可以扩展为更通用的类型
-}

divByGeneric ::  (Monad m, Integral a) => a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = fail "division by zero in divBy"
divByGeneric n (x:xs) = do
    next <- divByGeneric n xs
    return ((n `div` x) : next)

divBy4 :: Integral a => a -> [a] -> Maybe [a]
divBy4 = divByGeneric

{-
ghci> divBy 50 [1,2,5,8,10]
Just [50,25,10,6,5]
ghci> (divByGeneric 50 [1,2,5,8,10])::(Integral a => Maybe [a])
Just [50,25,10,6,5]

如果我们不指定类型，ghc默认Monad为IO monad

ghci> divByGeneric 50 [1,2,5,8,10]
[50,25,10,6,5]
ghci> divByGeneric 50 [1,2,0,8,10]
*** Exception: user error (division by zero in divByGeneric)
-}

data DivByError a
    = DivBy0
    | ForbiddenDenominator a
    | OtherDivByError String
    deriving (Eq, Read, Show)

instance Error (DivByError a) where
    strMsg x = OtherDivByError x

divBy5 :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy5 = divByGeneric1

divByGeneric1 :: (Integral a, MonadError (DivByError a) m) => a -> [a] -> m [a]
divByGeneric1 _ [] = return []
divByGeneric1 _ (0:_) = throwError DivBy0
divByGeneric1 _ (10:_) = throwError (ForbiddenDenominator 10)
divByGeneric1 n (x:xs) = do
    next <- divByGeneric1 n xs
    return ((n `div` x) : next)
