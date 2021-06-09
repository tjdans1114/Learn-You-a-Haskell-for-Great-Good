myGCD :: Integral t => t -> t -> t
myGCD 0 b = abs b
myGCD a 0 = abs a
myGCD a b = myGCD b (a `rem` b)