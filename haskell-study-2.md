# 2. 함수의 구문

### 패턴매칭

- 어떤 데이터가 따라야 할 패턴을 지정하거나, 그 패턴에 따라 데이터를 분해하기 위해 사용됨.

- 패턴마다 함수의 body를 만들 수 있다.

- 숫자, 문자, 리스트 튜플 등 많은 데이터 타입에 대해 패턴 매칭이 가능하다.

- 패턴은 위부터 순서대로 검사되고, 데이터가 지정된 패턴을 따른다면 해당 함수 body가 실행된다.

- 패턴 안에 소문자로 시작하는 이름을 사용하면 포괄적 패턴으로 사용 가능

  - 포괄적 패턴을 맨 위에 위치시키면 그 다음 패턴들은 늘 무시될 것

  

```haskell
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  
```

- 1 ~ 5 범위의 인자에 대해서 각각에 대응하는 결과를 리턴하고, 그 외에는 "Not between 1 and 5"를 리턴하는 프로그램



- if, then, else 를 사용해서도 구현할 수 있지만 이게 훨씬 간단하고 가독성이 좋음

  ```haskell
  sayMe n =
    if n == 1
      then "One"
      else
        if n == 2
          then "Two"
          else
            if n == 3
              then "Three"
              else if n == 4 then "Four" else if n == 5 then "Five" else "Not between"
  ```



- 패턴 매칭 + 재귀적으로 factorial을 구현한 예

  ```haskell
  factorial :: (Integral a) => a -> a  
  factorial 0 = 1  
  factorial n = n * factorial (n - 1)  
  ```



- 끝에 포괄적인 패턴을 추가해주지 않아서 패턴 매칭에 실패할 경우 'Non-exhaustive patterns' 가 있다는 에러가 날 것이므로 주의하자



**문자**

```
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
```



**튜플**

- 패턴 매칭을 통해 튜플을 편리하게 분해해서 사용할 수 있음

```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b)  
```

```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  
```

- triple에서 패턴 매칭을 사용한 예

```
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  
```

- _: don't care variable, 해당 부분에 대해 신경 쓰지 않을 때 사용



**리스트와 리스트 통합**

```
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
```

- [x, y]는 x:y:[] 에 대한 syntactic sugar
- (x:[]) 는 [x], (x:y:[])는 [x, y]라고 쓸 수도 있음. - hlint는 이걸 좋아하더라..
- x:xs 는 헤더를 x에, 나머지 리스트를 xs에 바인딩
  - [1]을 패턴매칭하면 x = 1, xs = [] 이 됨

- 패턴 매칭에서는 ++를 사용할 수 없음

  - pattern match는 constuctor로만 가능함. - constructor는 일대일 함수.
  - ++ 는 function
  - xs ++ ys를 하나로 패턴매칭 할 수 없음.

- 리스트 통합

  ```haskell
  ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
  ghci> [a+b | (a,b) <- xs]  
  [4,7,6,8,11,4]   
  ```

  - 패턴 매칭이 실패한 항목은 결과 리스트에서 누락됨, 리스트 통합은 다음 항목으로 이동

**as-패턴**

- 전체 원본 항목을 참조할 때 사용

  ```haskell
  capital :: String -> String  
  capital "" = "Empty string, whoops!"  
  capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  
  
  ghci> capital "Dracula"  
  "The first letter of Dracula is D"  
  ```

  

### 가드

- 값들의 속성을 검사하여 조건에 따라 다른 값을 리턴하고 싶을 때 사용

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  
```

- 들여쓰기, |, 조건, 함수의 내용 순서
- otherwise: 위의 모든 가드가 False일 때 실행됨
- 적절한 가드가 없을 때도 에러가 발생



### where, let

- 값을 저장해뒀다가 쓰고 싶을 때 사용

**where**

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
```

- where 절을 통해 변수에 값을 바인딩할 수 있음

- equation 내에서만 볼 수 있다.

- 예시의 모든 가드에서는 볼 수 있다.

- where 절의 모든 변수명은 같은 칼럼에 배열할 것

- 서로 다른 패턴의 함수 body에서는 볼 수 없다.

  ```haskell
  greet :: String -> String
  greet "Juan" = niceGreeting ++ " Juan!" -- error
  greet "Fernando" = niceGreeting ++ " Fernando!" -- error
  greet name = niceGreeting ++ " " ++ name
    where
      niceGreeting = "Hey you"
      badGreeting = "Oh! Pfft. It's you."
  ```

- where 바인딩에서도 패턴 매칭이 가능

  ```haskell
  initials :: String -> String -> String  
  initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
      where (f:_) = firstname  
            (l:_) = lastname    
  ```

- where에서 함수도 정의 가능

  ```haskell
  calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
  calcBmis xs = [bmi w h | (w, h) <- xs]  
      where bmi weight height = weight / height ^ 2  
  ```



**let**

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

- `let <bindings> in <expression>` 형식
- `in <expression>` : 전체 표현식이 <expression> 값을 가질 것이라는 의미

- where 과의 차이

  - binding이 앞, 표현식이 뒤에 온다.

  - scope가 매우 작음. 가드에서도 안 보임.

  - where binding은 문법적 구조(syntactic constructs)인 데 반해 let bindings는 그 자체로 expression이기 때문에 코드 어디서든 사용할 수 있음 (아래와 같은 게 가능)

    ```haskell
    ghci> 4 * (let a = 9 in a + 1) + 2  
    42  
    
    ghci> [let square x = x * x in (square 5, square 3, square 2)]  
    [(25,9,4)]  
    ```

- binding 여러 개를 한 줄에 쓸 때는 세미콜론(;)으로 구분, 줄바꿈할 때는 ; 빼야함

  ```
  ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
  (6000000,"Hey there!")  
  ```

  

- 튜플을 해체해서 각각의 요소들에 변수를 바인딩할 때 유용

  ```
  ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100  
  600  
  ```



- where binding은 함수 body 다음에 정의됨. 따라서 함수 body와 함수 이름 및 타입의 거리가 가깝게 해주며 가독성을 높여주는 장점이 있음.



**list comprehension에서의 let**

- list comprehension에 let binding을 사용해서 let binding 이후와 출력 결과(| 앞)가 볼 수 있는 변수를 만들 수 있음

  ```haskell
  calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
  calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]  
  ```

  - let binding 앞부분에서는 bmi 변수를 참조할 수 없음. 여기서는 generator 부분 (`(w, h) <- xs`)

- do block과 list comprehension에서는 `in` 없이 사용, 나머지 경우에는 `let ... in ...` 형태로 사용한다. 
  - https://stackoverflow.com/a/8274846

  - GHCi에서 in 없이 let binding을 생성하면 인터랙티브 세션 내내 보이게 될 이름을 만들 수 있다.

    ```haskell
    ghci> let zoot x y z = x * y + z  
    ghci> zoot 3 9 2  
    29  
    ghci> let boot x y z = x * y + z in boot 3 4 2  
    14  
    ghci> boot  
    <interactive>:1:0: Not in scope: `boot'  
    ```

    - in을 포함하면 그 자체로 expression이 되기 때문에 GHCi는 값을 출력하게 되고, 해당 바인딩에서 만들어진 이름은 사라진다.



### case expression

- 특정 변수의 특정 값에 대한 코드 블록을 실행할 수 있게 해줌
- 그 자체로 expression이다.

```
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
```

```
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  
```

- 매개변수에 대해 case expression을 사용하는 것은 앞에서 나온 패턴 매칭과 동일한 작업이다. 사실 아래는 위의 syntactic sugar이다.

```
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  
```



- 표현식이므로 어디서든 사용될 수 있다.

```haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
```

```haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
```

- where 절에서 함수를 패턴매칭을 통해 정의함으로써 동일한 동작을 구현할 수 있음.





# 재귀 Recursion

- 재귀적으로 정의된 함수 - 자기자신을 호출
- 더 이상 나눠지지 않고, 명확하고 재귀적이지 않은 base case와 inductive case를 선언해서 계산을 수행
- haskell은 lazy하게 계산하기 때문에 base case가 없는 재귀함수도 선언해서 유용하게 사용 가능
- 패턴매칭을 이용하면 깔끔하게 구현 가능



**maximum**

```haskell
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  
```



**take**

```haskell
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  
```



**repeat**

```haskell
repeat' :: a -> [a]  
repeat' x = x:repeat' x  -- 그냥 호출하면 무한의 리스트를 반환하지만

take 5 (repeat 3) -- 같은 게 가능
```



**quicksort**

```haskell
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  
```

