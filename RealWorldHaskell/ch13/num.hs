{-

重载运算符，实现各种计算输出控制

-- 正常计算
ghci> 5 + 1 * 3
8
-- 输出带优先级()的算式
ghci> prettyShow $ 5 + 1 * 3
"5+(1*3)"
ghci> prettyShow $ 5 * 1 + 3
"(5*1)+3"
-- 可以化简掉 (+0) (*1) (`div`1) 等无意义的计算
ghci> prettyShow $ simplify $ 5 + 1 * 3
"5+3"
-- 逆波兰表达式
ghci> rpnShow $ 5 + 1 * 3
"5 1 3 * +"
-- 同样可以对逆波兰表达式化简
ghci> rpnShow $ simplify $ 5 + 1 * 3
"5 3 +"
-- 可以带入变量
ghci> prettyShow $ 5 + (Symbol "x") * 3
"5+(x*3)"
-- 可以携带单位
ghci> 5 / 2
2.5
ghci> (units 5 "m") / (units 2 "s")
2.5_m/s
-- 可以正确处理不能计算的单位
ghci> (units 5 "m") + (units 2 "s")
*** Exception: Mis-matched units in add
ghci> (units 5 "m") + (units 2 "m")
7_m
ghci> (units 5 "m") / 2
2.5_m
ghci> 10 * (units 5 "m") / (units 2 "s")
25.0_m/s
-- 表达式可以正常赋值
ghci> test
13
ghci> rpnShow test
"2 5 * 3 +"
ghci> prettyShow test
"(2*5)+3"
ghci> test + 5
18
ghci> prettyShow (test + 5)
"((2*5)+3)+5"
ghci> rpnShow (test + 5)
"2 5 * 3 + 5 +"
-- 可以处理三角函数
ghci> sin (pi / 2)
1.0
ghci> sin (units (pi / 2) "rad")
1.0_1.0
ghci> sin (units 90 "deg")
1.0_1.0
ghci> (units 50 "m") * sin (units 90 "deg")
50.0_m
ghci> ((units 50 "m") * sin (units 90 "deg")) :: Units (SymbolicManip Double)
50.0*sin(((2.0*pi)*90.0)/360.0)_m
ghci> prettyShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
"50.0*sin(((2.0*pi)*90.0)/360.0)"
ghci> rpnShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
"50.0 2.0 pi * 90.0 * 360.0 / sin *"
ghci> (units (Symbol "x") "m") * sin (units 90 "deg")
x*sin(((2.0*pi)*90.0)/360.0)_m
-}


import Data.List

{-
haskell 提供的重载操作符的方式是 定义一个新的类型，然后实现instance
我们通过这个将操作符转换成类型来完成操作符识别，
然后我们就可以在此类型的基础上实现我们想要的功能
-}
data Op =
      Plus
    | Minus
    | Mul
    | Div
    | Pow
    deriving (Eq, Show)

data SymbolicManip a =
      Number a
    | Symbol String
    | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
    | UnaryArith String (SymbolicManip a)
    deriving (Eq)

-- 将各种操作符转换为类型
instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)

instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a

prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
    in  pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) = opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen x@(Number _) = prettyShow x
simpleParen x@(Symbol _) = prettyShow x
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow a

rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i =
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
        toList (UnaryArith op a) = toList a ++ [op]
        join :: [a] -> [[a]] -> [a]
        join delim l = concat (intersperse delim l)
    in  join " " (toList i)

simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
    let sa = simplify ia
        sb = simplify ib
    in case (op, sa, sb) of
        (Mul, Number 1, b) -> b
        (Mul, a, Number 1) -> a
        (Mul, Number 0, b) -> Number 0
        (Mul, a, Number 0) -> Number 0
        (Div, a, Number 1) -> a
        (Plus, a, Number 0) -> a
        (Plus, Number 0, b) -> b
        (Minus, a, Number 0) -> a
        _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

--data Num a => Units a = Units a (SymbolicManip a)
data Units a = Units a (SymbolicManip a)
    deriving (Eq)

instance (Num a, Eq a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add or subtract"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

instance (Fractional a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)

    instance (Floating a) => Floating (Units a) where
        pi = (Units pi (Number 1))
        exp _ = error "exp not yet implemented in Units"
        log _ = error "log not yet implemented in Units"
        (Units xa ua) ** (Units xb ub)
            | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
            | otherwise = error "units for RHS of ** not supported"
        sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
        sin (Units xa ua)
            | ua == Symbol "rad" = Units (sin xa) (Number 1)
            | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
            | otherwise = error "Units for sin must be deg or rad"
        cos (Units xa ua)
            | ua == Symbol "rad" = Units (cos xa) (Number 1)
            | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
            | otherwise = error "Units for cos must be deg or rad"
        tan (Units xa ua)
            | ua == Symbol "rad" = Units (tan xa) (Number 1)
            | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
            | otherwise = error "Units for tan must be deg or rad"
        asin (Units xa ua)
            | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
            | otherwise = error "Units for asin must be empty"
        acos (Units xa ua)
            | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
            | otherwise = error "Units for acos must be empty"
        atan (Units xa ua)
            | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
            | otherwise = error "Units for atan must be empty"
        sinh = error "sinh not yet implemented in Units"
        cosh = error "cosh not yet implemented in Units"
        tanh = error "tanh not yet implemented in Units"
        asinh = error "asinh not yet implemented in Units"
        acosh = error "acosh not yet implemented in Units"
        atanh = error "atanh not yet implemented in Units"

units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)

dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x

deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)

instance (Show a, Num a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

test :: (Num a) => a
test = 2 * 5 + 3
