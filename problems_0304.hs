-- 1
myLast :: [a] -> a
myLast [] = error "cannot get last element of empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "Error"
myButLast [x] = error "Error"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! n

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 7

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x : y : xs)
  | x == y = next
  | otherwise = x : next
  where
    next = compress (y : xs)

-- 9

-- 10
