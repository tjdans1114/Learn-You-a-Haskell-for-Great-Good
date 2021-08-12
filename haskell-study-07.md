# 10. 함수적으로 문제 해결하기

### RPN 계산기

- 역 폴란드 기법(reverse polish notation)으로 대수식을 쓰면, 연산자는 숫자들 뒤로 간다
    - 3 + 4 → 3 4 +

- `10 4 3 + 2 * -`

![https://s3-us-west-2.amazonaws.com/secure.notion-static.com/40e6e6fd-df17-4c7f-a889-403524d74b90/Screen_Shot_2021-07-14_at_6.40.14_PM.png](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/40e6e6fd-df17-4c7f-a889-403524d74b90/Screen_Shot_2021-07-14_at_6.40.14_PM.png)

- 숫자를 만나면 push
- 연산자를 만나면 스택 상단의 2개를 pop해서 그 숫자들에 연산자를 사용하고, 결과를 스택에 push
- 표현식의 끝에 다다르면(표현식이 올바르다면) 스택에는 결과를 나타내는 단 하나의 숫자가 남을 것
- 프로토타입

```haskell
import Data.List  
  
solveRPN :: (Num a) => String -> a  
solveRPN expression = head (foldl foldingFunction [] (words expression))  
    where   foldingFunction stack item = ...

-- 또는
import Data.List  
  
solveRPN :: (Num a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction stack item = ...
```

```haskell
solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs
```

- 연산자를 더 추가

```haskell
import Data.List  
  
solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs
```

- 확장이 쉽다

히드로 공항 문제는 각자 읽어보기ㅠ

# 11. Applicative Functor

### 펑터의 귀환

- 펑터는 리스트, Maybe, 트리처럼 매핑될 수 있음
    - 단 하나의 타입 클래스 메서드 fmap을 갖는 타입 클래스
    - `fmap :: (a -> b) -> f a -> f b`
    - a를 받아서 b를 반환하는 함수, 안에 a가 있는 상자를 받아 b가 있는 상자를 반환한다
    - 상자 안에 있는 요소에 (a → b)를 적용한다(map)
- 펑터값 - 추가된 컨텍스트(context)를 가진 값
    - fmap은 컨텍스트를 유지하면서 그 값에 함수를 적용한다
    - Maybe는 실패한 extra context를 가짐
    - 리스트에서의 컨텍스트는 한 번에 몇 가지 값들이 되거나 아무것도 없는 값이 될 수 있는 값
- type constructor를 가지고 Functor의 인스턴스를 만들고 싶다면, 그 type constructor는 타입 매개변수로 하나의 구체적인 타입을 받는다는 의미인 * → * 를 가져야 함
    - Maybe 가능: Maybe String, Maybe Int
    - Either 불가능: Either a b

    (이런 경우에는 단 하나의 타입 매개변수만 받도록 하기위해 타입 생성자를 부분적으로 적용해야 한다)

    - so, Either a 가능

        ```haskell
        instance Functor (Either a) where
        	fmap :: (b -> c) -> Either a b -> Either a c
        ```

### IO is instance of Functor

- I/O 작업이 가져온 것을 조사할 수 있지만, 그런 다음 그 값을 I/O로 다시 래핑해야 함 → 펑터처럼 동작함

    ```haskell
    instance Functor IO where  
        fmap f action = do  
            result <- action  
            return (f result)
    ```

    ```haskell
    -- 입력을 받아 거꾸로 출력해주는 프로그램
    main = do line <- getLine   
              let line' = reverse line  
              putStrLn $ "You said " ++ line' ++ " backwards!"  
              putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

    -- functor를 사용하여 다시 작성한 코드
    main = do line <- fmap reverse getLine  
              putStrLn $ "You said " ++ line ++ " backwards!"  
              putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
    ```

    - `fmap reverse getLine` 은 결과값이 뒤집힌다는 것만 제외하면 getLine과 동일하게 동작
    - `fmap :: (a -> b) -> IO a -> IO b`
    - fmap은 함수와 IO작업을 받아서 새로운 IO작업을 반환한다.
    - 함수가 결과에 적용된다는 것을 제외하면 이전 것과 동일하게 동작함
    - 단지 함수를 적용하고, 다른 이름으로 부르기 위해(`←` ) IO 작업의 결과에 이름을 바인딩하려고 한다면 fmap 사용을 고려해보면 좋을 것
    - 여러 함수를 적용하고 싶으면 상위 레벨에 함수를 선언하거나, 람다 함수를 만들거나, 합성 함수를 사용하면 됨

        ```haskell
        import Data.Char  
        import Data.List  
          
        main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
                  putStrLn line

        $ runhaskell fmapping_io.hs  
        hello there  
        E-R-E-H-T- -O-L-L-E-H
        ```

### `(→) r` is instance of Functor

- 함수 타입인 `r -> a` 는 `(-r) r a` 처럼 다시 쓸 수 있음
- 두 개의 타입 매개변수를 받는 타입 생성자이므로, 타입 생성자를 부분적으로 적용해야 Functor로 만들 수 있다.
    - `(->) r`
- Control.Monad.Instances의 함수 펑터

    ```haskell
    instance Functor ((->) r) where  
        fmap f g = (\x -> f (g x))
    ```

```haskell
fmap :: (a -> b) -> f a -> f b

-- 각각의 f를 (->) r 로 바꿈
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

- 함수를 Maybe에 매핑 → Maybe를 생성
- 함수를 리스트에 매핑 → 리스트를 생성
- 함수를 함수에 매핑 → 함수를 생성해야 한다.
- a → b 함수와 r → a 함수를 받아서 r → b 함수를 반환 ⇒ 합성함수
- 따라서 펑터 인스턴스 `(->) r` 은 아래와 같이 작성될 수도 있음

    ```haskell
    instance Functor ((->) r) where  
        fmap = (.)
    ```

    ```haskell
    ghci> :t fmap (*3) (+100)  
    fmap (*3) (+100) :: (Num a) => a -> a  
    ghci> fmap (*3) (+100) 1  
    303  
    ghci> (*3) `fmap` (+100) $ 1  
    303  
    ghci> (*3) . (+100) $ 1  
    303  
    ghci> fmap (show . (*3)) (*100) 1  
    "300"
    ```

    - `(+100)`에 `fmap (*3)`을 사용하는 것은 (`(*3)` 을 매핑하는 것은 `(+100)` 과 같이 동작하는 또 다른 함수를 만들어 내는 것이다. 하지만 결과를 만들기 전에 `( *3)` 이 그 결과에 적용될 것

### fmap을 바라보는 두 가지 관점

```haskell
fmap :: (a -> b) -> f a -> f b

-- or

fmap :: (a -> b) -> (f a -> f b)
```

1. 함수와 펑터값을 받아서 그 펑터값에 그 함수를 매핑하는 함수
2. 함수를 받아서 그 함수를 lifting하는 함수. 펑터값에서 동작하는 함수.

```haskell
ghci> :t fmap (replicate 3)  
fmap (replicate 3) :: (Functor f) => f a -> f [a]

ghci> fmap (replicate 3) [1,2,3,4]  
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
ghci> fmap (replicate 3) (Just 4)  
Just [4,4,4]  
ghci> fmap (replicate 3) (Right "blah")  
Right ["blah","blah","blah"]  
ghci> fmap (replicate 3) Nothing  
Nothing  
ghci> fmap (replicate 3) (Left "foo")  
Left "foo"
```

### 펑터 규칙

- 펑터에서 fmap을 호출하기 위해서는 펑터에 함수를 매핑해야 한다.
- 이 동작은 **펑터 규칙**으로 묘사됨
- Functor의 모든 instance들은 두 가지 규칙을 준수해야 함.
- 표준 라이브러리에 있는 모든 Functor instance는 이 규칙들을 따름.
- 이 법칙들의 역할은 fmap이 정상적으로 작동하여 매핑 작업을 실제로 수행하도록 보장하는 것

1. `fmap id = id`
    - 펑터값에 id 함수를 매핑하면 돌려받은 펑터값은 원본 펑터값과 동일해야 한다.
    - 펑터값에 fmap id를 적용하는 것은 그 값에 그냥 id를 적용하는 것과 동일해야 한다.

    ```haskell
    ghci> fmap id (Just 3)  
    Just 3  
    ghci> id (Just 3)  
    Just 3  
    ghci> fmap id [1..5]  
    [1,2,3,4,5]  
    ghci> id [1..5]  
    [1,2,3,4,5]  
    ghci> fmap id []  
    []  
    ghci> fmap id Nothing  
    Nothing
    ```

2. `fmap (f . g) = fmap f . fmap g` 또는 `fmap (f . g) x = fmap f (fmap g x)`
    - 두 개의 함수를 합한 다음에 합한 함수를 펑터에 매핑하는 것은 펑터에 하나의 함수를 먼저 매핑한 다음에 다른 함수를 매핑하는 것과 동일해야 한다.
    - 다시 말해, 합성 함수를 매핑하든 어떤 함수를 먼저 매핑하고 다른 함수를 매핑하든 무관해야 한다는 것
- 어떤 타입이 두 개의 펑터 규칙을 모두 따른다는 것을 보여줄 수 있다면, 매핑에 관한 다른 펑터들처럼 동일한 기본 동작을 한다고 할 수 있다.

### Applicative Functor

- Control.Applicative 모듈의 Applicative 타입 클래스

```haskell
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b
```

- Applicative  타입 클래스의 타입 생성자는 일단 Functor여야 함
- 어떤 타입 생성자가 Applicative의 Instance라면, 항상 Functor이므로 fmap을 할 수 있음
- pure
    - 모든 타입의 값을 받아 어플리커티브 값을 반환 (결과로 그 값을 갖는 어플리커티브 값으로 래핑)
    - 값을 받아서 그것을 최소한의 컨텍스트에 넣는다.
- <*>
    - 일종의 강화된 fmap
    - 그 안에 함수를 가진 펑터값과 또 다른 펑터를 받아서 첫 번째 펑터에서 함수를 추출하여 두 번째 펑터에 매핑

### Maybe Applicative Functor

```haskell
instance Applicative Maybe where  
    pure = Just  -- pure x = Just x 라고 쓸 수도 있음
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something
```

- <*>
    - Nothing 안에는 아무 함수가 없으므로 함수를 추출할 수 없음. 결과는 Nothing이 된다
    - 왼쪽 매개변수가 Just라면 함수를 추출해서 오른쪽 매개변수에 매핑한다.

### 어플리커티브 스타일

```haskell
ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing
```

- `pure f <*> x <*> y <*> ...`
    - 어플리커티브 값이 아닌 매개변수를 받는 함수를 받아서, 여러 개의 어플리커티브 값에서 동작하기 위해 그 함수를 사용할수 있게 해줌
    - <*>가 처리되는 사이마다 부분적으로 적용되기때문에, 두 개 이상의 매개 변수를 받는 함수를 사용할 수 있다.
- `pure f <*> x` 는 `fmap f x` 와 같다
    - 디폴트 컨텍스트에 들어간 함수를 추출하여 두번째 매개변수인 어플리커티브 펑터에 매핑하는 것과, 함수를 두번째 매개변수에 매핑하는 것은 동일
    - `pure f <*> *x <*>* y <*> ...` 과 `fmap f x <*> *y <*>* ...` 은 같다.
- <$>

    ```haskell
    (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
    f <$> x = fmap f x
    ```

    - 세 개의 어플리커티브 펑터 간에 함수 f를 적용하고자 할 경우에

        ```haskell
        f <$> x <*> y <*> z
        ```

        이렇게 쓸 수 있다.

        - 어플리커티브 펑터가 아닌 일반값이었다면 `f x y z` 라고 썼을 것!

    ```haskell
    ghci> (++) <$> Just "johntra" <*> Just "volta"  
    Just "johntravolta"

    ghci> (++) "johntra" "volta"  
    "johntravolta"
    ```

    - <$>와 <*>를 포함시키면 일반 함수가 어플리커티브들에서 동작하여 하나의 어플리커티브를 반환하게 할 수 있다!

### List

- List는 Applicative Functor다 (와)

```haskell
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]
```

- pure는 값을 받아서 그 값을 결과로 갖는 최소한의 컨텍스트에 담에 반환한다.
- 리스트의 최소한의 컨텍스트는 빈 리스트라고 할 수 있지만, 빈 리스트는 값이 없음을 나타내므로 pure에 사용한 값을 가지고 있을 수 없음.
- 그래서 리스트 어플리커티브의 pure는 값을 받아 그 값 하나를 요소로 갖는 singleton list를 반환한다.
    - Maybe의 pure가 Nothing이 아닌 Just인 것도 같은 이유에서임
- pure의 실제 사용 예

    ```haskell
    ghci> pure "Hey" :: [String]  
    ["Hey"]  
    ghci> pure "Hey" :: Maybe String  
    Just "Hey"
    ```

- <*>
    - 리스트 어플리커티브에 대해 타입은 `(<*>) :: [a -> b] -> [a] -> [b]` 와 같아야 할 것
    - list comprehension을 통해 구현됨
    - 왼쪽의 모든 함수를 오른쪽의 모든 값에 적용한다.

    ```haskell
    ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
    [0,0,0,101,102,103,1,4,9]

    ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
    [4,5,5,6,3,4,6,8]

    -- [(1+),(2+),(1*),(2*)] <*> [3,4]
    ```

- 어플리커티브 스타일의 사용은 list comprehension의 좋은 대체가 될 수 있다

    ```haskell
    ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]     
    [16,20,22,40,50,55,80,100,110]

    ghci> (*) <$> [2,5,10] <*> [8,10,11]  
    [16,20,22,40,50,55,80,100,110]

    ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  
    [55,80,100,110]
    ```

### IO

- IO도 어플리커티브 펑터다

```haskell
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)
```

- <*>
    - IO 동작인 a를 받아서 함수를 내어놓고, 그 함수를 수행하고 결과를 f에 바인딩
    - IO 동작인 b를 수행하고 결과를 x에 바인딩
    - f를 x에 적용해서 결과로 내어 놓음
    - `(<*>) :: IO (a -> b) -> IO a -> IO b`
    - IO에서는 순차(sequencing)의 개념이 추가되었다. 두 개의 IO 동작을 받아서 하나로 합치기 때문.
    - 첫번째 IO 동작에서 함수를 추출해야 하는데, 결과를 추출하기 위해서는 그 IO 동작을 수행해야 함.

    ```haskell
    myAction :: IO String  
    myAction = do  
        a <- getLine  
        b <- getLine  
        return $ a ++ b

    -- Applicative style로 이렇게 쓸 수 있다
    myAction :: IO String  
    myAction = (++) <$> getLine <*> getLine
    ```

    - 표현식 `(++) <$> getLine <*> getLine` 의 타입은 IO String이다. 완벽하게 일반적인 IO 동작이고, 다른 IO 동작처럼 결과값을 내놓음
    - 따라서 아래와 같은 것도 가능

        ```haskell
        main = do  
            a <- (++) <$> getLine <*> getLine  
            putStrLn $ "The two lines concatenated turn out to be: " ++ a
        ```

### Applicative인 함수

- `(->) r` 도 어플리커티브의 인스턴스다
- 일반적으로 잘 사용하지 않음

```haskell
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)
```

- pure
    - 어떤 값을 pure를 가지고 Applicative 값으로 wrapping할 때, 이것이 내어놓는 결과는 그 '어떤 값'이어야 한다.
    - 따라서 함수에 특화된 pure는
        - `pure :: a -> (r -> a)`

        ```haskell
        ghci> (pure 3) "blah"  
        3
        ```

- <*>

    ```haskell
    ghci> :t (+) <$> (+3) <*> (*100)  
    (+) <$> (+3) <*> (*100) :: (Num a) => a -> a  
    ghci> (+) <$> (+3) <*> (*100) $ 5  
    508
    ```

    - `(+) <$> (+3) <*> (*100)` 인자를 받아서 (+3)과 ( *100)을 계산한 결과에 +를 사용한 것을 반환하는 함수를 반환

    ```haskell
    ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
    [8.0,10.0,2.5]
    ```

### ZipList

```haskell
instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

- <*>
    - zipWith가 동작하는 방식에 의해 결과리스트는 두 리스트 중 더 길이가 짧은 것의 길이를 갖게 될 것
- pure
    - 모든 위치의 값을 만들어야하기 때문에 무한 리스트가 되어야 한다.
    - `pure f <*> xs` should equal `fmap f xs` 의 규칙에도 맞다
    - `pure (*2) <*>* ZipList [1,5,10]`

### 어플리커티브 규칙

```haskell
-- 가장 중요
pure f <*> xs = fmap f xs

그 외에는 책 참고
```

### 유용한 함수들

- liftA2

    ```haskell
    liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
    liftA2 f a b = f <$> a <*> b
    ```

    ```haskell
    ghci> liftA2 (:) (Just 3) (Just [4])  
    Just [3,4]  
    ghci> (:) <$> Just 3 <*> Just [4]  
    Just [3,4]
    ```

- 어플리커티브 값들의 리스트를 받아서 결과값으로 리스트를 갖는 어플리커티브 값을 반환하는 함수 구현하기 (sequenceA)

    ```haskell
    sequenceA :: (Applicative f) => [f a] -> f [a]  
    sequenceA [] = pure []  
    sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
    ```

    ```haskell
    ghci> sequenceA [Just 1, Just 2]
    Just [1, 2]
    ```

    - fold를 이용한 구현

    ```haskell
    sequenceA :: (Applicative f) => [f a] -> f [a]  
    sequenceA = foldr (liftA2 (:)) (pure [])
    ```

    - pure []에서 시작해서 마지막 요소(어플리커티브)를 가지고 liftA2 (:)를 호출함

    ```haskell
    ghci> sequenceA [Just 3, Just 2, Just 1]  
    Just [3,2,1]  
    ghci> sequenceA [Just 3, Nothing, Just 1]  
    Nothing  
    ghci> sequenceA [(+3),(+2),(+1)] 3  
    [6,5,4]  
    ghci> sequenceA [[1,2,3],[4,5,6]]  
    [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
    ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]  
    []
    ```

    - 함수 어플리커티브와 사용되면, sequenceA는 함수들의 리스트를 받아서 리스트를 반환하는 함수를 반환
    - 함수들의 리스트를 가지고 있으며, 그 함수들 전체에 동일한 입력을 제공한 결과 리스트를 보고자 할 때 유용
    - 어떤 숫자가 리스트에 있는 조건을 모두 만족하는지 확인

        ```haskell
        ghci> map (\f -> f 7) [(>4),(<10),odd]  
        [True,True,True]  
        ghci> and $ map (\f -> f 7) [(>4),(<10),odd]  
        True

        ghci> sequenceA [(>4),(<10),odd] 7  
        [True,True,True]  
        ghci> and $ sequenceA [(>4),(<10),odd] 7  
        True
        ```

    - IO에 사용되면 sequence와 동일

        ```haskell
        ghci> sequenceA [getLine, getLine, getLine]  
        heyh  
        ho  
        woo  
        ["heyh","ho","woo"]
        ```

        - IO 동작들의 리스트를 받아서 결과의 리스트를 결과로 갖게 될 IO 동작을 반환.
        - [IO a] 값을 IO [a]로 바꾸고 실행될 때 결과들의 리스트를 내어놓는 IO 동작을 만들기 때무넹 하나가 끝난 다음 다른 것이 수행되도록 모든 IO 동작들은 순서대로 되어야 함

- 어플리커티브 스타일을이용하면, 일관되게 동작하는 일반 함수를 어플리커티브 펑터의 값에 적용할 수 있다.