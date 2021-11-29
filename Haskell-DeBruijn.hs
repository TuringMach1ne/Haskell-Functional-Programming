



-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort (take n xs)) (msort (drop n xs))
  where
    n = div (length xs) 2


-------------------------

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term

instance Show Term where
  show = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

example1 :: Term
example1 = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeral' i))
  where
    numeral' i
      | i <= 0    = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i-1))

variables :: [String]
variables = map (:[]) ['a'..'z'] ++ [ x : show i | i <- [1..] , x <- ['a'..'z'] ]

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = filter (/=x) (free n)
free (Apply  n m) = merge (free n) (free m)


------------------------- Assignment 1

data TermDB =
     VariableDB Int
     | LambdaDB TermDB
     | ApplyDB TermDB TermDB
     deriving Eq


instance Show TermDB where
  show = f 0
    where
      f i (VariableDB x) = show x
      f i (LambdaDB m)   = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\." ++ f 0 m 
      f i (ApplyDB n m)  = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

example2 :: TermDB
example2 = LambdaDB (ApplyDB (LambdaDB (LambdaDB (ApplyDB (VariableDB 1) (ApplyDB (LambdaDB (VariableDB 0)) (VariableDB 5))))) (ApplyDB (LambdaDB (VariableDB 1)) (VariableDB 1)))


depth :: TermDB -> Int
depth (VariableDB v) = 0
depth (LambdaDB t)   = 1 + depth t
depth (ApplyDB w u)  = max (depth w) (depth u)


------------------------- Assignment 2

--Error handling isn't instructed in the coursework specification.

giveNames :: [Var] -> [Var] -> TermDB -> Term
giveNames a b (VariableDB i)    = Variable (b !! i)
giveNames (x:xs) b (LambdaDB m) = Lambda x (giveNames xs (x:b) m)
giveNames a b (ApplyDB m p)     = Apply (giveNames a b m) (giveNames a b p)



named :: TermDB -> Term
named n = giveNames a b n
  where
    a = take (depth n) variables
    b = drop (depth n) variables

------------------------- Assignment 3

indexOf :: Eq a => a -> [a] -> Int
indexOf e [] = error "cannot find element" --element doesn't occur
indexOf e (a:as)
     | e == a  = 0
     | e /= a  = 1 + indexOf e as



deBruijnList :: [Var] -> Term -> TermDB
deBruijnList a (Variable v) = VariableDB (indexOf v a)
deBruijnList a (Lambda v t) = LambdaDB (deBruijnList (v:a) t) 
deBruijnList a (Apply t u) = ApplyDB (deBruijnList a t) (deBruijnList a u) 

deBruijn :: Term -> TermDB
deBruijn t = deBruijnList (free t) t



------------------------- Assignment 4

lift :: Int -> TermDB -> TermDB
lift i t = aux 0 i t
  where
     aux :: Int -> Int -> TermDB -> TermDB
     aux f i (VariableDB x)
          | x >= f          = VariableDB $ x + i
          | otherwise      = VariableDB x
     aux f i (LambdaDB t)  = LambdaDB $ aux (f+1) i t
     aux f i (ApplyDB y u) = ApplyDB (aux f i y) (aux f i u)



substitute :: Int -> TermDB -> TermDB -> TermDB
substitute j n (VariableDB x)
     | j == x  = n
     | otherwise = VariableDB x
substitute j n (LambdaDB t) = LambdaDB $ substitute j n t
substitute j n (ApplyDB y u) = ApplyDB (substitute j n y) (substitute j n u)

beta :: TermDB -> [TermDB]
beta = undefined

normalize :: TermDB -> IO ()
normalize = undefined




------------------------- Assignment 5

isalpha :: Term -> Term -> Bool
isalpha t1 t2 = t1' == t2' && free t1 == free t2
  where
    t1' = deBruijn t1
    t2' = deBruijn t2 
