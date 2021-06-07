-- 99 Questions
-- Chapter 3 & 4
-- Jaebok Shin

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

-- (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

-- Problem 8
-- λ> compress "aaaabccaadeeee"
-- "abcade"
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (cutReplicates x xs)
  where
    cutReplicates _ [] = []
    cutReplicates h t@(y : ys)
      | h == y = cutReplicates h ys
      | otherwise = t

-- auxiliary function
splitPrefix :: Eq a => ([a], [a]) -> ([a], [a])
splitPrefix res@(_, []) = res
splitPrefix res@([], _) = res
splitPrefix res@(prefix@(x : xs), y : ys)
  | x == y = splitPrefix (y : prefix, ys)
  | otherwise = res

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x : xs) =
  let (prefix, suffix) = splitPrefix ([x], xs)
   in prefix : pack suffix

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x : xs) =
  let (prefix, suffix) = splitPrefix ([x], xs)
   in (length prefix, head prefix) : encode suffix

-- Problem 11
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
data ElemDescription a = Single a | Multiple Int a

instance Show a => Show (ElemDescription a) where
  show (Single x) = "Single " ++ show x
  show (Multiple n x) = "Multiple " ++ show n ++ " " ++ show x

encodeModified :: Eq a => [a] -> [ElemDescription a]
encodeModified [] = []
encodeModified (x : xs) =
  let (prefix, suffix) = splitPrefix ([x], xs)
   in ( if length prefix == 1
          then Single (head prefix)
          else Multiple (length prefix) (head prefix)
      ) :
      encodeModified suffix

-- auxiliary
convertToElemDesc :: Int -> a -> ElemDescription a
convertToElemDesc n elem
  | n == 0 = error "elem count should be greater than 1"
  | n == 1 = Single elem
  | otherwise = Multiple n elem

encodeModified' :: Eq a => [a] -> [ElemDescription a]
encodeModified' [] = []
encodeModified' (x : xs) =
  let (prefix, suffix) = splitPrefix ([x], xs)
   in convertToElemDesc (length prefix) (head prefix) : encodeModified' suffix

-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
-- λ> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [ElemDescription a] -> [a]
decodeModified = concatMap convertElemDescToList
  where
    convertElemDescToList :: ElemDescription a -> [a]
    convertElemDescToList (Single x) = [x]
    convertElemDescToList (Multiple n x) = replicate n x

-- decodeModified [] = []
-- decodeModified (x : xs) = convertElemDescToList x ++ decodeModified xs
--decodeModified = foldr ((++) . convertElemDescToList) []

-- Problem 13
encodeDirect :: Eq a => [a] -> [ElemDescription a]
encodeDirect [] = []
encodeDirect (x : xs) =
  let (desc, remained) = countConsecutives x 1 xs
   in desc : encodeDirect remained
  where
    countConsecutives :: Eq a => a -> Int -> [a] -> (ElemDescription a, [a])
    countConsecutives elem count [] = (convertToElemDesc count elem, [])
    countConsecutives elem count l@(x : xs)
      | elem == x = countConsecutives elem (succ count) xs
      | otherwise = (convertToElemDesc count elem, l)

-- Problem 26
-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]
combinations' :: Int -> [a] -> [[a]]
combinations' 0 _ = []
combinations' n x
  | n > length x = []
combinations' 1 (x : xs) = [x] : combinations' 1 xs
combinations' n (x : xs) =
  map (x :) (combinations' (n - 1) xs) ++ combinations' n xs

combinations :: Int -> [a] -> [[a]]
combinations n x = filter ((==) n . length) (combinations' n x)

-- Problem 31
-- λ> isPrime 7
-- True
isPrime :: Int -> Bool
isPrime n
  | n < 2 = error "n should be greater than 1"
  | n == 2 || n == 3 = True
  | even n || n `mod` 3 == 0 = False
  | otherwise = auxtest n 5
  where
    auxtest :: Int -> Int -> Bool
    auxtest n i
      | i ^ 2 > n = True
      | n `mod` i == 0 || n `mod` (i + 2) == 0 = False
      | otherwise = auxtest n (i + 6)

-- Problem 32
-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
myGCD :: Int -> Int -> Int
myGCD n m
  | n == 0 = m
  | otherwise = myGCD (m `mod` n) n

-- Problem 33
-- Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1
coprime :: Int -> Int -> Bool
coprime n m = myGCD n m == 1

-- Problem 34
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
-- λ> totient 10
-- 4
totient :: Int -> Int
totient m = length [x | x <- [1 .. (m - 1)], coprime m x]

-- Problem 35
-- λ> primeFactors 315
-- [3, 3, 5, 7]
primeFactors :: Int -> [Int]
primeFactors n
  | n < 2 = []
  | n == 2 || n == 3 = [n]
  | otherwise = let f = findSmallestFactor n 2 in f : primeFactors (n `quot` f)
  where
    findSmallestFactor :: Int -> Int -> Int
    findSmallestFactor n 2
      | even n = 2
      | isPrime n = n
      | otherwise = findSmallestFactor n 3
    findSmallestFactor n f
      | n < f = n
      | isPrime f && n `mod` f == 0 = f
      | otherwise = findSmallestFactor n (f + 2)

-- Problem 36
-- λ> prime_factors_mult 315
-- [3, 3, 5, 7]
-- [(3,2),(5,1),(7,1)]
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = encodeList (primeFactors n)
  where
    encodeList :: [Int] -> [(Int, Int)]
    encodeList [] = []
    encodeList l@(x : xs) =
      let count = elemCounter x l
       in (x, count) : encodeList (drop count l)

    elemCounter :: Int -> [Int] -> Int
    elemCounter _ [] = 0
    elemCounter elem (x : xs)
      | elem == x = succ (elemCounter elem xs)
      | otherwise = 0

-- Problem 37
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--          (p2 - 1) * p2 ** (m2 - 1) *
--          (p3 - 1) * p3 ** (m3 - 1) * ...
phi :: Int -> Int
phi m = phi' (primeFactorsMult m)
  where
    phi' :: [(Int, Int)] -> Int
    phi' [] = 1
    phi' ((p', m') : xs) = (p' - 1) * p' ^ (m' -1) * phi' xs

-- Problem 39
-- λ> primesR 10 20
-- [11,13,17,19]
primesR :: Int -> Int -> [Int]
primesR f e = [x | x <- [f .. e], isPrime x]

-- Problem 40
-- Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
-- Example: 28 = 5 + 23.
-- λ> goldbach 28
-- (5, 23)
goldBach :: Int -> (Int, Int)
goldBach m
  | odd m = error "only even number"
  | m == 2 = error "greater than 2"
  | otherwise = twoSum' (primesR 2 (m - 1)) m
  where
    twoSum' :: [Int] -> Int -> (Int, Int)
    twoSum' [] _ = error "Goldbach is wrong??"
    twoSum' (x : xs) target
      | x + x == target = (x, x)
      | (target - x) `elem` xs = (x, target - x)
      | otherwise = twoSum' xs target

-- Problem 41
-- λ> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList f e = [goldBach x | x <- [f .. e], even x]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' f e lb = [(p1, p2) | x <- [f .. e], even x, let (p1, p2) = goldBach x, p1 > 50, p2 > 50]
