myLast :: [a] -> a
myLast [] = error "fail"
myLast [x] = x
myLast (_ : xs) = myLast xs
