doubleElems :: Num t => [t] -> [t]
doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs


sqrElems [] = []
sqrElems (x:xs) = x^2 : sqrElems xs

-- evalFuncListAt :: a -> [a -> b] -> [b]
-- evalFuncListAt x = map (id x)