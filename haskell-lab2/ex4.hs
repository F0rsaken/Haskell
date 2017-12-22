import Data.Char
isPalindrome :: [Char] -> Bool
isPalindrome s = if s == reverse s then True else False

getElemAtIdx :: (Int,[a]) -> a
getElemAtIdx (i,list)
    | i == 0 = head list
    | i == (length list - 1) = last list
    | otherwise = head (drop i list)

capitalize :: [Char] -> [Char]
capitalize w = [toUpper (head w)] ++ tail w

isPrime :: Int -> Bool
-- isPrime n 
--     | n < 1 = False
--     | otherwise = [i | i <- [2..n-1], n `mod` i == 0] == []

isPrime n = if n `elem` primes then True else False

countPrimes :: Int -> Int
countPrimes n = length [x | x <- [1..n], isPrime x]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

