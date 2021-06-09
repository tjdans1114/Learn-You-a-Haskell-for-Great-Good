-- very very naive
isPrime :: Integral a => a -> Bool
isPrime n = length [x | x <- [1 .. n], gcd n x == 1] == 1