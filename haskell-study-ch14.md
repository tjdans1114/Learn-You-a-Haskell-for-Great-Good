# More Monads

모나드가 컨텍스트를 가진 값들을 받아서 함수에 전달하는 방법을 배웠으며,
하스켈이 컨텍스트를 처리하는 동안 `>>=` 와 `do 표기법`을 이용하면 그 값에 초점을 맞출 수 있다.

- Maybe 모나드
- 리스트 모나드
- IO 모나드
- and more ...
 
## Writer 모나드 
- 로그를 컨텍스트로 가지는 계산
- 디버깅 목적, 계산 순서 설명 등등
 
### 도입 (applyLog)
```haskell
isBigGang :: Int -> Bool
isBigGang x = x > 9
```

```haskell
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")
```

주어진 값이 (input 이) `(3, "Smallish gang.")` 또는 `(30, "A freaking platoon)` 이라면??

```haskell
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
```

결과:
```
ghci> (3, "Smallish gang.") `applyLog` isBigGang
(False,"Smallish gang.Compared gang size to 9")

ghci> (30, "A freaking platoon.") `applyLog` isBigGang
(True,"A freaking platoon.Compared gang size to 9")
```

### Monoid 를 활용한 일반화
- (a, String) 에서 String 이 아닌 List 혹은 바이트스트링, Sum, Product, ... 가 될 수 있음
- String/List 에서 `++` 연산자를 사용: 다른 타입 공통으로 사용할 수 있는 것은 `mappend`
- applyLog 에 모노이드를 도입하자!
 
```haskell
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)
```

- 예제 (Sum Int)
```
ghci> Sum 3 `mappend` Sum 9
Sum (getSum = 13)
```
- 음식 주문에 음료를 추가하는 예제
```haskell
import Data.Monoid  
  
type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)
```
- 실행 결과
```
ghci> ("beans", Sum 10) `applyLog` addDrink
("milk",Sum {getSum = 35})
ghci> ("jerky", Sum 25) `applyLog` addDrink
("whiskey",Sum {getSum = 124})
ghci> ("dogmeat", Sum 5) `applyLog` addDrink
("beer",Sum {getSum = 35})

ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink  
("beer",Sum {getSum = 65})`
```

### Writer 모나드 타입
- Control.Monad.Writer 모듈에 `Writer w a` 타입이 정의 되어있음

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

- 모나드 인스턴스 정의
- (*) return: 하나의 값을 받아서 그 값을 포함하는 최소 컨텍스트로 표현 (mempty: "", Sum 0 등)
```haskell
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
``` 

### Writer 로 do 표기법 이용하기
- 여러개의 Writer 값을 가지고 작업을 할 경우에 편리함.
- 컨텍스트가 유지되면서 일반값을 이용한 계산처럼 표현 가능.
- 컨텍스트는 `mappend` 됨.

```haskell
import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  
```

- 실행 결과
```
ghci> runWriter multWithLog
(15, ["Got number: 3","Got number 5"])
```

- 어떤 모노이드 값 (컨텍스트) 값을 특정 시점에 포함하려면 `tell` 을 사용
- 그 값은 dummy ()를 가지는 Writer가 생성.
```haskell 
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)
```
- 실행 결과
```
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5","Gonna multiply these two"]) 
```
 
### 로그를 프로그램에 추가하기

```haskell
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)
```

- 실행 단계 설명을 출력하도록 해보자!

```haskell
import Control.Monad.Writer  
  
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)
```

- 실행결과
```
ghci> runWriter (gcd' 8 3)
(1,["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"])

ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
8 mod 3 = 2
3 mod 2 = 1
2 mod 1 = 0
Finished with 1
```

### 비효율적인 리스트 연산

- 실행 로그를 역순으로 출력하고 싶다:
```haskell
import Control.Monad.Writer  
  
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result
```

- 실행 결과
```
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)  
Finished with 1  
2 mod 1 = 0  
3 mod 2 = 1  
8 mod 3 = 2
```

- `gcd'` 의 계산 순서 : `a ++ (b ++ (c ++ (d ++ (e ++ f))))`
- `gcdReverse` 의 계산 순서 : `((((a ++ b) ++ c) ++ d) ++ e) ++ f`

### Difference List 사용하기

- 리스트를 argument로 받아서 그 앞에 다른 리스트를 붙임
- [a] -> [a]
- ex. [1, 2, 3]의 difference list 는 \xs -> [1, 2, 3] ++ xs
- ex. ("meat" ++) 또는(\xs -> "meat" ++ xs)
- ex. ("abc" ++) `append` ("def" ++) : \xs -> "abc" ++ ("def" ++ xs)
- ex. 빈 difference list: \xs -> [] ++ xs

- 타입 구분을 위해...
```haskell
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f [] 

-- monoid instance
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
```

- DiffList 모노이드의 동작
```
ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])  
[1,2,3,4,1,2,3] 
```

- getReverse 의 효율성을 높여보자!
```haskell
import Control.Monad.Writer  
  
gcdReverse' :: Int -> Int -> Writer (DiffList String) Int  
gcdReverse' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcd' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result
```

- 0 에서 500000 까지 숫자 출력해보기
- 비교:
```haskell
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])
    
-- vs.

finalCountDown :: Int -> Writer [String] ()  
finalCountDown 0 = do  
    tell ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell [show x]
```

## Reader 모나드 

### 함수도 모나드!

- 함수도 컨텍스트를 가진 값이라고 볼 수 있음.
- 그 컨텍스트는 그 값이 아직 나타나지 않았다는 것과
  함수를 어떤 것에 적용해야 그 값을 얻을 수 있다는 것
 
```haskell
instance Monad ((->) r) where  
    return x = \_ -> x   -- 최소 컨텍스트 (매개변수 무시)
    h >>= f = \w -> f (h w) w
```

- 모나드 >>= (일반값 -> 모나드) = 모나드
- h w: 일반 값
- f (h w - 일반 값): 모나드
- f (h w) w : 결과 값
- 결국, 일반값을 받아서 결과값을 뱉는 함수 (모나드)
- 함수를 다른 함수에 전달하면 그 결과도 함수다!

### 리더 모나드

```haskell
import Control.Monad.Instances  
  
addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)
    b <- (+10)
    return (a+b)
```
- 실행 결과
```
ghci> addStuff 3
(3 * 2) + (3 + 10)
```
- 모든 함수는 공통의 소스를 읽는다 => Reader 모나드
- 함수들을 하나의 함수로 합치고, 그 함수의 매개변수를 그것을 구성하는 모든 함수들에 전달.

## 세련되게 상태를 유지하는 계산 (State 모나드)
- 순수 언어: 계산을 하고 그 결과를 반환 - 외부 상태 수정 없음
- 문제의 종류에 따라 시간이 지나면서 변경되는 상태에 의존함.

```haskell
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)
```

- Stateful 문제 해결 방식을 지원하기 위한 State 모나드

### 상태를 유지하는 계산
- Stateful 계산: `s -> (a, s)`

### 스택 예제
```haskell
type Stack = [Int]  
  
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)
  
push :: Int -> Stack -> ((), Stack)  
push a xs = ((), a:xs) 
```

- 간단한 스택 조작 함수
```haskell
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2
```

- 실행 결과
```
ghci> stackManip [5,8,2,1]  
(5, [8,2,1])
```

- State 모나드를 사용한다면?
```haskell
stackManip = do  
    push 3  
    a <- pop  
    pop  
```

### State 모나드

```haskell
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState
```
- 모나드 >>= (일반값 -> 모나드) = 모나드
- stateful 계산 >>= (일반값 -> stateful 계산) = stateful 계산

- Stack 예제에 적용
```haskell
import Control.Monad.State  
  
pop :: State Stack Int  
pop = State $ \(x:xs) -> (x, xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((), a:xs)


-- 위 stackManip 에 적용
import Control.Monad.State  
  
stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop 
```

- 약간 복잡한 작업
```haskell
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()            
```

### State 모나드 와 RandomGen
- System.Random 의 random 함수:
```haskell
random :: (RandomGen g, Random a) => g -> (a, g)
```

- 이것을 State 모나드와 연결
```haskell
import System.Random  
import Control.Monad.State  
  
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random
```

```haskell
import System.Random  
import Control.Monad.State  
  
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c)
```

## 몇 가지 유용한 모나드 함수들
- 모나드 값을 처리하거나, 결과로 모나드 값을 반환하거나 두 가지 모두 하는 함수들
- liftM, join, filterM, foldM

### liftM
- functor의 fmap
```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b

fmap :: (Functor f) => (a -> b) -> f a -> f b

ghci> liftM (*3) (Just 8)  
Just 24

ghci> runWriter $ liftM not $ Writer (True, "chickpeas")  
(False,"chickpeas")
```

### join

- 모나드 값이 중첩된다면 이것을 하나의 모나드 값으로 펼칠 수 있을까?
- Just (Just 9) => Just 9?

```haskell
join :: (Monad m) => m (m a) -> m a

ghci> join (Just (Just 9))
Just 9

ghci> join (Just Nothing)
Nothing

ghci> join [[1,2,3],[4,5,6]]  
[1,2,3,4,5,6]
```

### filterM

- 조건식 (predicate) 과 리스트를 받아서 조건에 부합하는 리스트를 반환
```haskell
filter :: (a -> Bool) -> [a] -> [a]

-- 모나드에 대해서는
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  
```

- 리스트 필터링 예제
```haskell
ghci> filter (\x -> x < 4) [9,1,5,2,10,3]  
[1,2,3]
```

- True, False 뿐만 아니라 무슨 작업을 했는지에 대한 로그도 보고싶다!
- keepSmall (a -> m Bool)
```haskell
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False
```

- 실행 결과
```
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
9 is too large, throwing it away  
Keeping 1  
5 is too large, throwing it away  
Keeping 2  
10 is too large, throwing it away  
Keeping 3  
```

### foldM

- foldl의 모나드 버전
 
```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```

- 숫자들의 리스트의 합을 내고 싶은데 리스트에 있는 숫자들 중에 9보다 더 큰 숫자가 있을 경우 전체가 실패하게 하고 싶다. 
- fold를 진행하다가 9보다 큰 수를 만나면 실패 ==> Maybe 의 Nothing
```haskell
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)
```

- 실행 결과
```
ghci> foldM binSmalls 0 [2,8,3,1]  
Just 14  
ghci> foldM binSmalls 0 [2,11,3,1]  
Nothing
```
