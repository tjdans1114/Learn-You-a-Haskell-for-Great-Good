compress :: Eq a => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a : b : xs)
  | a == b = compress (b : xs)
  | otherwise = a : compress (b : xs)