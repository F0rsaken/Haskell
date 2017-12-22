sgn :: Int -> Int
sgn x
    | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1

min3 :: (Int, Int, Int) -> Int
min3 (x, y, z) 
    | x < y && x < z = x
    | y < z = y
    | otherwise = z

