sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum    = sumWith (\x -> x)
sumSqr = sumWith (\x -> x^2)
sumCube = sumWith (\x -> x^3)
sumAbs = sumWith (\x -> abs x)

listLength = sumWith (\x -> 1)

prod' :: Num a => [a] -> a
prod' [] = 0
prod' [x] = x
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 0
prodWith f [x] = f x
prodWith f (x:xs) = f x * prodWith f xs

prod     = prodWith (\x -> x)
prodSqr  = prodWith (\x -> x^2)
prodCube = prodWith (\x -> x^3)
prodAbs  = prodWith (\x -> abs x)

--skończone na 2.7 - o chuj chodzi?