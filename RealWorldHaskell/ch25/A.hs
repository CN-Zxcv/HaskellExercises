{-# LANGUAGE BangPatterns #-}

import System.Environment
import Text.Printf
import Data.List

import Control.Parallel.Strategies

main = do
    [d] <- map read `fmap` getArgs
    printf "%f\n" (mean [1..d])

--mean :: [Double] -> Double
--mean xs = sum xs / fromIntegral (length xs)

{-
-- -rtsopts 接收运行时参数
$> ghc --make -rtsopts -O2 A.hs
$> time ./A 1e5
-- 运行时间信息
real    0m0.055s
user    0m0.016s
sys     0m0.031s

-- 在标准错误中输出详细运行信息
$> ./A 1e7 +RTS -sstderr
... 886 MB total memory in use
... %GC     time      83.2%  (84.0% elapsed)
-}

{-
$> ghc -O2 --make A.hs -prof -auto-all -fprof-cafs -fforce-recomp
-prof 性能剥析标志，获取基本的时间空间消耗信息,
    -- 可以通过在函数内加上{-# SCC "name" #-} 的方式让prof观察到某函数
    -auto-all 将所有的顶层函数设置为消耗集中点,让prof观察到
    -fprof-cafs 常量函数体(Constant Applicative Forms) 执行一次的消耗
-fforce-recomp 强制重新编译
-}

{-
time ./A 1e6 +RTS -K100M -p -hc

-hc 堆内存剥析
-hy 性能剥析
-hd 构造器剥析

-}

{-
严格求值方式1
使用foldl'
并且对foldl'的结果参数应用seq
-}
mean :: [Double] -> Double
mean xs = s / fromIntegral n
    where
        (n, s) = foldl' k (0, 0) xs
        k (n, s) x = n `seq` s `seq` (n + 1, s + x)

{-
更有保证的是使用求值策略
但是使用求值策略后无法依赖默认的类型推导了，需要手动添加类型注释
-}
mean1 :: [Double] -> Double
mean1 xs = s / fromIntegral n
    where
        (n, s) = foldl'rnf k (0, 0) xs
        k (n, s) x = (n + 1, s + x) :: (Int, Double)

foldl'rnf :: NFData a => (a -> b -> a) -> a -> [b] -> a
foldl'rnf f z xs = lgo z xs
    where
        lgo z [] = z
        log z (x:xs) = lgo z' xs
            where
                z' = f z x `using` rdeepseq

{-
使用扩展 -XBangPatterns
-}
mean2 :: [Double] -> Double
mean2 xs = s / fromIntegral n
    where
        (n, s) = foldl' k (0, 0) xs
        k (!n, !s) x = (n + 1, x + s)

{-
利用 BangPatterns 定义严格数据类型,
任何放在Pair中的数据都会被求值到WHNF
-}
data Pair a b = Pair !a !b
mean3 :: [Double] -> Double
mean3 xs = s / fromIntegral n
    where
        Pair n s = foldl' k (Pair 0 0) xs
        k (Pair n s) x = Pair (n + 1) (x + s)
