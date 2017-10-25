min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x > y
    then y else x

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = min2Int(min2Int(x, y), z)

toUpper :: Char -> Char
toUpper x = toEnum(fromEnum(x) - 32)

toLower :: Char -> Char
toLower x = toEnum(fromEnum(x) + 32)
