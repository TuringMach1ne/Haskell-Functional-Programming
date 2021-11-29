--Q1 Specs

(+++) :: [a] -> [a] -> [a]
[] +++ ys        = ys
(x:xs) +++ ys    = x : (xs +++ ys)


concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = xs ++ concat2 xss

fun1 :: [[a]] -> [a]
fun1   = foldl (++) []



fun2 :: [a] -> [a] -> [a]
x `fun2` y    = foldr (:) y x



fun3  :: [[a]] -> [a]
fun3 xs = [xss | xss <- fun1 xs]

