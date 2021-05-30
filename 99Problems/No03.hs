elementAt :: [a] -> Int -> a
elementAt _ k
  | k < 1 = error "fail"
elementAt [] _ = error "fail"
elementAt (x : _) 1 = x
elementAt (_ : xs) k = elementAt xs (k -1)
