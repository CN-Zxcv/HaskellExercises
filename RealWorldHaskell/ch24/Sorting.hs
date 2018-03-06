
module Sorting where

import Control.Parallel (par, pseq)

sort :: (Ord a) => [a] -> [a]
sort (x:xs) = lesser ++ x:greater
    where
        lesser = sort [y | y <- xs, y < x]
        greater = sort [y | y <- xs, y >= x]
sort _ = []

parSort :: (Ord a) => [a] -> [a]
parSort (x:xs) = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where
        lesser = parSort [y | y <- xs, y < x]
        greater = parSort [y | y <- xs, y >= x]
parSort _ = []

force :: [a] -> ()
force xs = go xs `pseq` ()
    where
        go (_:xs) = go xs
        go [] = 1

parSort2 :: (Ord a) => Int -> [a] -> [a]
parSort2 d list@(x:xs)
    | d <= 0 = sort list
    | otherwise = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where
        lesser = parSort2 d' [y | y <- xs, y < x]
        greater = parSort2 d' [y | y <- xs, y >= x]
        d' = d - 1
parSort2 _ _ = []

{-
par
将左侧参数求值为弱首范式(WHNF)并返回右侧参数
par不保证左右参数的求值顺序

pseq
将左侧参数求值为弱首范式(WHNF)并返回右侧参数
保证左侧先执行

force
强制将表达式计算为NF,
以上parSort如果不加force,那么并行计算时，列表计算最外层后就停止了,并没有有效并行
当然这种情况也仅仅是遍历了列表,并不保证列表中的元素会被计算
-}

sillySort (x:xs) = greater `par` (lesser `pseq` (lesser ++ x:greater))
  where lesser   = sillySort [y | y <- xs, y <  x]
        greater  = sillySort [y | y <- xs, y >= x]
sillySort _        = []

{-
** 范式NF
完全规约后的表达式
如:
    42
    (2, "hello")
    \x -> (x + 1)
而以下的不是
    1 + 2
    (\x -> x + 1) 2
    "he" ++ "llo"
    (1 + 1, 2 + 2)

** 首范式HNF
    对一般数据来说，弱首范式和首范式相同。它们仅仅在功能上有些许区别
    TODO:

** 弱首范式WHNF
表达式最外面一层已经规约，内部无所谓
如:
    (1 + 1, 2 + 2)          -- 最外层是(,)
    \x -> 2 + 2             -- 最外层是\x ->
    'h' : ("e" ++ "llo")    -- 最外层是 (:)
而以下不是
    1 + 2                   -- 最外层是(+)
    (\x -> x + 1) 2         -- 最外层是(\x -> x + 1)
    "he" ++ "llo"           -- 最外层是(++)
-}
