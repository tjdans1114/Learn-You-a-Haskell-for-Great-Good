myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = aux []
  where
    aux acc [] = acc
    aux acc (x : xs) = aux (x : acc) xs