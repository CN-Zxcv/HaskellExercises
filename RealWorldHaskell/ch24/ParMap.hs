import Control.Parallel

{-
相比命令式语言的主要关注点在线程通信上
haskell 并行编程主要关注点在par pseq的频繁使用上，
我们希望有更好的表示方式
-}

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x:xs) =
    let r = f x
    in  r `par` r : parallelMap f xs
parallelMap _ _ = []
{-
如果我们需要实现一个并行计算列表，将使用如上的方式,
但是我们希望更通用一点，而不是对每种类型都定制并行计算函数,
-}

{-
所以我们先关注如何强制求值
对于列表,我们的办法是遍历列表，对列表元素应用pseq
-}
forceList :: [a] -> ()
forceList (x:xs) = x `pseq` forceList xs
forceList _ = ()

stricterMap :: (a -> b) -> [a] -> [b]
stricterMap f xs = forceList xs `seq` map f xs

{-
通过引入一个求值函数并遍历元素对元素强制求值
-}
forceListAndElts :: (a -> ()) -> [a] -> ()
forceListAndElts forceElt (x:xs) =
    forceElt x `seq` forceListAndElts forceElt xs
forceListAndElts _ _ = ()

{-
Control.Parallel.Strategies

type Done = ()
type Strategy a = a -> Done

-- 不求值
r0 :: Strategy a
r0 _ = ()

-- WHNF
rwhnf :: Strategy a
rwhnf x = x `seq` ()

class NFData a where
    rnf :: Strategy a
    rnf = rwhnf

instance NFData Char
instance NFData Int

instance NFData a => NFData (Maybe a) where
    rnf Nothing = ()
    rnf (Just x) = rnf x
-}

{-
列表求值策略
-}
parList :: Strategy a -> Strategy [a]
parList strat [] = ()
parList strat (x:xs) = strat x `par` (parList strat xs)

{-
通过using & parList
将map计算的算法和求值策略分离
-}
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat

using :: a -> Strategy a -> a
using x s = s x `seq` x
