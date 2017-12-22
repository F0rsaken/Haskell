import Data.Char

onlyEven :: Integral t => [t] -> [t]
onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd :: Integral t => [t] -> [t]
onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 1 = x : onlyOdd xs
    | otherwise = onlyOdd xs

onlyUpper :: [Char] -> [Char]
onlyUpper [] = []
onlyUpper (x:xs)
    | isUpper x = x : onlyUpper xs
    | otherwise = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

onlyEven' = filter' (\x -> x `mod` 2 == 0)
onlyOdd' = filter' (\x -> x `mod` 2 == 1)
onlyUpper' = filter' isUpper