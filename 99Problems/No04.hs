myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = aux 0
  where
    aux k [] = k
    aux k (_ : xs) = aux (k + 1) xs