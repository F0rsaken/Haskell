roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c)
    | d > 0 = ( (-b - d) / e, (-b + d) / e )
    | d == 0 = (-b / e)
    | otherwise = -1
    where
        d = sqrt(b*b - 4 * a * c)
        e = 2*a

-- unit2Vec :: (Double, Double) -> (Double, Double)

