myButLast :: [a] -> a
myButLast = last . init

myButLast' :: [a] -> a
myButLast' [] = error "fail"
myButLast' [_] = error "fail"
myButLast' [x, _] = x
myButLast' (_ : xs) = myButLast xs