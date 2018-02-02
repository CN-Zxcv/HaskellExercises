
foldl' _ zero [] = zero
foldl' step zero (x:xs) =
    let new = step zero x
    in new `seq` foldl' step new xs

-- foldl' (+) 1 (2:[])
-- =
-- let new = 1 + 2
-- in new `seq` foldl' (+) new []
-- =
-- foldl' (+) 3 []
-- =
-- 3

-- 关于seq的使用
-- seq在执行时会强制第一个参数进行规约
-- 但是如果被放到了闭包里面，只有闭包被使用到了的时候才会调用seq，
-- 这可能和我们想使用的不一致

someFunc = undefined
anotherFunc = undefined

-- (x `seq` y) 在someFunc中被使用到时才对x规约
hiddenInside x y = someFunc (x `seq` y)

-- 同理anotherFunc中使用到了a时才对x规约
hiddenByLet x y z =
    let a = x `seq` someFunc y
    in anotherFunc a z

-- 放在最外面，x在第一时间规约
onTheOutside x y = x `seq` someFunc y

chained x y z = x `seq` y `seq` someFunc z

-- 这种情况 (step zero x) 只有第一个会被规约
badExpression step zero (x:xs) = seq (step zero x) (badExpression step (step zero x) xs)
