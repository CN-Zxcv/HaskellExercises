
-- Excercise of https://deque.blog/2017/02/03/code-your-own-quickcheck/
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid
import System.Random
import Data.Foldable (foldMap)
import Debug.Trace
import Data.List
import Text.Show.Functions

-- 对(...) -> Bool 类型的函数生成随机值进行测试,返回测试结果

-- 首先定义quickCheck的返回
data Result
    = Success                        -- 成功
    | Failure                        -- 失败
        { seed :: Int                -- 随机种子，用来重复随机值
        , counterExample :: [String] -- 失败值
        } deriving (Eq, Show, Ord)

-- 辅助函数，用来处理失败数据
-- :: a -> (a -> b) -> b
overFailure :: Result -> (Result -> Result) -> Result
overFailure Success _ = Success
overFailure failure f = f failure

-- 对于测试, 希望任何情况下失败，整体就返回失败
-- 这个性质可以定义为幺半群
-- 所以可以在Monoid上实现
instance Monoid Result where
    mempty = Success
    mappend lhs@Failure{} _ = lhs
    mappend _ rhs = rhs

-- 然后是输入

-- 输入使用随机生成，
-- 我们需求是从外部传入种子，所以定义为一个func
newtype Gen a = Gen {
    runGen :: StdGen -> a
}

-- 对于不同类型，需要自定义随机生成器
class Arbitrary a where
    arbitrary :: Gen a

instance Arbitrary Int where
    arbitrary = Gen $ \rand -> fst (next rand)

instance Arbitrary Integer where
    arbitrary = Gen $ \rand -> fromIntegral (fst (next rand))

instance Arbitrary Bool where
    arbitrary = Gen $ \rand -> even (fst (next rand))

--instance Arbitrary [a] where
--    arbitrary = Gen $ \rand -> runGen (perturb 0 rand)

-- 属性是通过随机产生值来验证
-- 从这里说，属性依赖于随机输入
-- 因此可以将属性定义为 (Generator -> Result)
-- 我们已经有了Gen,所以可以定义为如下形式
newtype Property = Property
    { getGen :: Gen Result
    }

-- 对属性应用generator时，有个辅助函数比较方便
runProp :: Property -> StdGen -> Result
runProp prop rand = runGen (getGen prop) rand

-- 有了以上的定义，我们可以开始实现 (Property -> Result)

-- 首先需要解决的是变长参数的问题
-- FP中多参函数可以看成如下形式
-- (a -> (b -> (c -> d)))
-- 所以变长参数可以通过值类与一元函数组成

-- 值
class Testable a where
    property :: a -> Property

instance Testable Property where
    property = id

instance Testable Result where
    property r = Property (Gen (const r))

instance Testable Bool where
    property = property . toResult where
        toResult b = if b
            then Success
            else Failure {seed = 0, counterExample = []}

-- 一元函数
instance (Show a, Arbitrary a, Testable testable) => Testable (a -> testable) where
    property f = forAll arbitrary f

-- forAll 递归对prop应用generator直到返回一个Normal Form
forAll :: (Show a, Testable testable) => Gen a -> (a -> testable) -> Property
forAll argGen prop = Property $ Gen $ \rand ->
    let (rand1, rand2) = split rand
        arg = runGen argGen rand1
        subProp = property (prop arg)
        result = runProp subProp rand2
    in overFailure result $ \failure ->
            failure {counterExample = show arg : counterExample failure}

fakeQuickCheckImpl :: Testable prop => Int -> Int -> prop -> Result
fakeQuickCheckImpl attemptNb startSeed prop = runAll (property prop)
    where
        runAll prop = foldMap (runOne prop) [startSeed .. startSeed + attemptNb - 1]
        runOne prop seed =
            let result = runProp prop (mkStdGen seed)
            in overFailure result $ \failure -> failure {seed = seed}

fakeQuickCheck :: Testable prop => prop -> IO Result
fakeQuickCheck = fakeQuickCheckWith 100

fakeQuickCheckWith :: Testable prop => Int -> prop -> IO Result
fakeQuickCheckWith attemptNb prop = do
    seed <- randomIO
    return $ fakeQuickCheckImpl attemptNb seed prop

-- prop_gcd :: Integer -> Integer -> Bool
-- prop_gcd a b = a * b == gcd a b * lcm a b
--
-- prop_gcd_bad :: Integer -> Integer -> Bool
-- prop_gcd_bad a b = gcd a b > 1

-- fakeQuickCheck prop_gcd
--  = Success

-- fakeQuickCheck prop_gcd_bad
--  = Failure {seed = xxx, counterExample = ["1299775935","1958068072"]}

-- 高阶函数支持的关键在于如何生成函数，
-- 如下的例子中我们的目标就是要实现 Gen (a -> b) 这样的形式
-- 即 StdGen -> (a -> b)
-- 如果我们重新排列输入顺序 a -> StdGen -> b
-- 那么可以看到我们其实只需要实现 a -> Gen b

-- 如果有a -> Gen b ,我们就可以定义一个转换函数将(a -> Gen b)转换为我们想要的形式
promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen $ \rand a -> runGen (f a) rand

-- 对于如何实现 a -> Gen b,从一个类型生成另外一个类型的Gen是不行的
-- 可行的的方式就是使用部分应用函数 (Gen b -> (a -> Gen b))
-- 通过Gen b 生成 (a -> Gen b)
class CoArbitrary a where
    coarbitrary :: Gen b -> a -> Gen b

-- 这样的话，高阶函数就可以这样定义
instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
    arbitrary = promote (coarbitrary arbitrary)

instance CoArbitrary Integer where
    coarbitrary gen n = Gen $ \rand -> runGen gen (perturb n rand)

instance CoArbitrary [Int] where
    coarbitrary gen xs = Gen $ \rand -> runGen gen (foldr perturb (perturb 0 rand) xs)

perturb :: (Integral n) => n -> StdGen -> StdGen
perturb n rand0 = foldl (\rand b -> vary b rand) (vary (n < 0) rand0) (digits (abs n))
    where
        vary digit rand = (if digit then snd else fst) (split rand)
        digits = map ((== 0) . (`mod` 2)) . takeWhile (> 0) . iterate (`div` 2)

prop_partition :: [Integer] -> (Integer -> Bool) -> Bool
prop_partition xs p =
    let (lhs, rhs) = partition p xs
    in and
        [ all p lhs
        , not (any p rhs)
        , sort xs == sort (lhs ++ rhs)
        ]

--fakeQuickCheck prop_partition
--    = Success

prop_distributive :: Integer -> Integer -> (Integer -> Integer) -> Bool
prop_distributive a b f = f (a + b) == f a + f b

--fakeQuickCheck prop_distributive
--    = Failure {seed = xxx, counterExample = [xx,xx,"<function>"]}
