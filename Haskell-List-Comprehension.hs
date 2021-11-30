

------------------------- Exercise 1

doubles :: [Int] -> [Int]
doubles xs = [ x*2 | x <- xs ]

odds :: [Int] -> [Int]
odds xs = filter odd xs

odds2 :: [Int] -> [Int]
odds2 xs = [ x | x <- xs , odd x]

doubleodds :: [Int] -> [Int]
doubleodds xs = [ 2*x | x <- xs , odd x]

shorts :: [String] -> [String]
shorts xs = [x | x <- xs , length x <=5 ]

squarePositives :: [Int] -> [Int]
squarePositives xs = [ x*x | x <- xs , 0 < x]

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums xss = [sum xs | xs <- xss , odd (length xs)]

remove :: Eq a => [a] -> a -> [a]
remove xs y = [ x | x <-xs , x/= y ]

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs ys = [x | x <- xs , not ( elem x ys) ]

everyother :: [a] -> [a]
everyother xs = [ x | (i,x) <- zip [1..] xs , odd i]

at :: [a] -> Int -> a
at xs i = head [ x | (e,x) <- zip [0..] xs , e==i ]

 
same :: Eq a => [a] -> [a] -> [Int]
same xs ys = [ i | (i,x,y) <- zip3 [1..] xs ys , x==y ]


------------------------- Exercise 2

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [ (x,y) | x <- xs , y <- ys ]

selfpairs :: [a] -> [(a,a)]
selfpairs xs = [ (x,y) | (i,x) <- zip [1..] xs , (j,y) <- zip [1..] xs , i<=j]

pyts :: Int -> [(Int,Int,Int)]
pyts n = [ (x,y,z) | x <- [1..n] , y <- [x..n] , z<- [y..n] , x*x+y*y == z*z ]

