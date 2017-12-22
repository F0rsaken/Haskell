sqr :: Num a => a -> a
sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = \x -> sum [(x^k)/fromIntegral(fact k) | k <- [0..n]]

fact :: (Num t, Ord t) => t -> t
fact = loop 1
    where loop acc 0 = 1
          loop acc 1 = acc
          loop acc n = loop (acc*n) (n-1)
-- fact n = if n < 2 then 1 else n * fact(n-1)

