fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

isDiv :: (Integral a, Eq a) => [a] -> Bool
isDiv (x : y : _) | (y `mod` x) == 0 = True
                  | otherwise = False