fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)

fib2 :: Int -> Int
fib2 n = last (take n fibs)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 0
prod' [x] = x
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' [x] = x
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = False
and' [x] = x
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' a [] = False
elem' a [x] = a == x
elem' a (x:xs) = a == x || elem' a xs

doubleAll :: Num t => [t] -> [t] -- double doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll [x] = [2*x]
doubleAll (x:xs) = (2*x:doubleAll xs)

squareAll :: Num t => [t] -> [t] -- double squareAll [2,3] = [4,9]
squareAll [] = []
squareAll [x] = [x^2]
squareAll (x:xs) = (x^2:squareAll xs)

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven [x] =
    if x `mod` 2 == 0 then [x] else []
selectEven (x:xs) = selectEven [x] ++ selectEven xs

sum'2 :: Num a => [a] -> a
sum'2 s = loop 0 s
    where loop acc [] = acc
          loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
    where loop acc [] = 0
          loop acc [x] = acc * x
          loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 = loop 0
    where loop acc [] = acc
          loop acc [x] = acc + 1
          loop acc (x:xs) = loop (1 + acc) xs