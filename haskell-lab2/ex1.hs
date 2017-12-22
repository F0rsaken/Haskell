myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

add2C :: Num a => a -> (a -> a)
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> (a -> (a -> a))
add3C x y z = x + y + z

curry2 :: ((a,b) -> c) -> a -> b -> c
curry2 f x y = f (x,y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a ,b) = f a b

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a ,b, c) -> d
uncurry3 f (x, y, z) = f x y z

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f x y = f y x