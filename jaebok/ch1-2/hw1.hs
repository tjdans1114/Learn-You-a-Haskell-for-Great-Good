-- 99 Questions
-- Chapter 1 & 2
-- Jaebok Shin

-- Problem 1
myLast :: [t] -> t
myLast = last

-- Problem 2
myButLast :: [t] -> t
myButLast = last . init

-- Problem 3
elementAt :: [t] -> Int -> t
elementAt list kth = list !! (kth - 1)

-- Problem 4
myLength :: [t] -> Int
myLength = length

-- Problem 5
myReverse :: [t] -> [t]
myReverse = reverse

-- Problem 6
isPalindrome :: Eq t => [t] -> Bool
isPalindrome l = l == reverse l

-- Problem 14
dupli :: [t] -> [t]
dupli l = concat [[e, e] | e <- l]

-- Problem 15
repli :: [t] -> Int -> [t]
repli l n = concat [replicate n x | x <- l]

-- Problem 16
dropEvery :: [t] -> Int -> [t]
dropEvery l n = [e | (i, e) <- zip [1 ..] l, i `mod` n /= 0]

-- Problem 17
split :: [t] -> Int -> ([t], [t])
--split l n = (take n l, drop n l)
split l n = splitAt n l

-- Problem 18
slice :: [t] -> Int -> Int -> [t]
slice l f e = take (e - f + 1) (drop (f - 1) l)

-- Problem 19
rotate :: [t] -> Int -> [t]
rotate l n = drop k l ++ take k l
  where
    k = if n < 0 then length l + n else n

-- Problem 20
removeAt :: Int -> [t] -> (t, [t])
removeAt k l = (l !! (k - 1), take (k - 1) l ++ drop k l)

-- Problem 21
insertAt :: t -> [t] -> Int -> [t]
insertAt e l n = take (n - 1) l ++ e : drop (n - 1) l

-- Problem 22
range :: Int -> Int -> [Int]
range f e = [f .. e]
