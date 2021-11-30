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

----------

deneme = do
  x <- getLine
  putStrLn x
  

hitme = do
    putStrLn ("Ninety-nine what ?")
    x <- getLine
    return (99,x)
    
    
pilate = do
    putStrLn ("What can I get you?")
    x <- getLine
    putStrLn ("Uh, we don't have " ++ x)
    pilate
    
    
countdown n = do
    print n
    if n == 0
    then do
        putStrLn "liftoff"
    else do
    countdown (n-1)
    
    
getList = do
    putStr "Who's on the list?"
    x <- getLine
    if x == "Nobody"
    then return []
    else do
        putStrLn (x ++ " is on the list!")
        xs <- getList
        return (x:xs)
