# Higher-Order Functions
- parameter나 return value가 함수인 함수
- JS에서도 익숙한 `Array.map` 등등

## Currying
- Haskell Curry의 이름에서 유래
- 하스켈의 모든 함수는 공식적으로 하나의 매개변수만을 사용
    - "Curried Function"
```haskell
Prelude> max 4 5
5
Prelude> (max 4) 5
5
Prelude> :t max
max :: Ord a => a -> a -> a -- Ord a => a -> (a -> a)
Prelude> :t (max 4)
(max 4) :: (Ord a, Num a) => a -> a
```
- type signature의 `->` 는 right-associative
- 내장 함수 `curry`, `uncurry`를 사용할 수도 있다.
```haskell
Prelude> myMax (x,y) = if x > y then x else y
Prelude> :t myMax
myMax :: Ord p => (p, p) -> p
Prelude> :t (curry myMax)
(curry myMax) :: Ord c => c -> c -> c
Prelude> :t (uncurry max)
(uncurry max) :: Ord c => (c, c) -> c
```

### 부분 적용 : Partial Application
- "여러 개의 인자를 받는 함수"에 적은 숫자의 인자를 사용해서 새로운 함수를 돌려받는 것
- 새로운 함수를 깔끔하게 생성할 수 있다.
```haskell
Prelude> max4 = (max 4)
Prelude> max4 5
5
Prelude> max4 3
4
```

### 섹션 : Section of an infix operator
- infix operator들의 partial application을 위한 syntax
- ``a `infix` b`` 일 때 (= `infix a b`)
    - left section : ``(a `infix`) b == a `infix` b``
        - ``(a `infix`) == infix a == \x → a infix x``
    - right section ``(`infix` b) a == a `infix ` b``
        - ``(infix b) = flip infix b = \x → x `infix` b``

```haskell
Prelude> (10 /) 2 -- == 10 / 2 == (/) 10 2
5.0
Prelude> (/ 10) 2 -- == 2 / 10 == (/) 2 10
0.2
```


### `($)` : function application operator
```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```
- `$` 는 '가장' 낮은 연산자 우선순위를 가짐
- 따라서 우리는 복잡한 괄호를 생략할 수 있다.
    - `f (g (h x))` 대신 `f $ g $ h x` 로
- `$` 가 들어가는 다른 operator들
    - `$!` : (lazy가 아닌) strict application을 강제함.
    - `<$`, `<$>` : Functor 관련

### `(.)` : function composition
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```
- 즉, `f (g x)` , `\x -> f (g x)` 를 각각 `(f . g) x`, `f . g` 로 대신 사용할 수 있다.

### point-free style (= tacit programming)
- 적절한 테크닉을 통해 새로운 함수의 parameter를 생략할 수 있다
    - function composition : `f x = f1 (f2 (f3 x))` 대신 `f = f1 . f2 . f3`
    - infix operator : `f x = 1 + x` 대신 `f  = (+) 1`
    - parameter flip : `f x = x + 1` 대신 `f = flip (+) 1`
    - 등등...
- 적절하게 사용한 경우 가독성이 높아지나, 너무 복잡한 경우 오히려 가독성이 떨어짐.

## map, filter
```haskell
map :: (a -> b) -> [a] -> [b]
map _ []  = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x = x: filter p xs
  | otherwise filter p xs
```
- 물론 list comprehension을 대신 사용할 수도 있다.

## fold (= reduce)
- `foldl`, `foldr`
```haskell
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```
- `foldl1`, `foldr1` : 초기값을 지정하는 대신 list head를 사용
```haskell
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
```
- special folds : `and`, `or`, `any`, `all`, `concat`, `concatMap`
```haskell
and :: Foldable t => t Bool -> Bool
or :: Foldable t => t Bool -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool
concat :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
```

### Infinite list와 fold
- `foldr` 은 무한 리스트와 사용 가능하고, `foldl`은 그렇지 않음
- evaluation을 잘 들여다보면 ...
```haskell
foldl f z [x1 ... xn] = f (... (f (f (f z x1) x2) x3) ...) xn

foldr f z [x1 ... xn] = f x1 (f x2 (f x3 (... (f xn z) ...)))
```
- haskell은 기본적으로 lazy한데,
    - `foldl`은 `f (...something...) xn` 형태이고
    - `foldr`은 `f x1 (...something...) 형태`라
    - `foldl`의 경우에 무한 리스트가 개입되면 partial application이 끝나지 않는다
- 따라서 무한 리스트를 fold할때는 `foldr`을 사용해야 한다
- Q. 그럼 `foldl`의 장점? : 유한 리스트일 때는 `foldl`이 tail recursive함.

### `foldl'` : 새로운 custom foldl
```haskell
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x : xs) =
    let z' = f z x
    in z' `seq` (foldl' f z' xs) -- `seq` forces reduction of z'
```
- strictness를 강제하는 새로운 `foldl'` 을 정의할 수 있다.
- lazy vs strict 차이 때문에 특별한 경우 동작이 달라질 수 있다.

### `foldl` vs `foldr` vs `foldl'`
- '대부분의 경우' `foldr`을 쓰면 알맞음
    - 무한 리스트를 대상으로 사용 가능
    - short-circuiting이 가능함. `foldl`, `foldl'` 는 불가
- `foldl` 은 비추
    - 대부분 `foldl'` 이 나은 선택.
    - implicit reverse의 이점을 활용해야 할 때도 `foldl'` 을 쓰는 것이 나음
- `foldl'` 이 `foldr` 보다 time/space performance가 좋은 경우가 많음
    - strict하기 때문에 heap allocation이 적음
    - short-circuiting이 불필요하고, finite하지만 매우 긴 list를 대상으로 작업할때 좋음.
    - commutative한 operation을 대상으로 할 때 좋음.

#### Scan
- `fold` 중간 결과들의 list
- 역시 `scanl`, `scanr`, `scanl1`, `scanlr` 버전이 있음

## References
- Haskell Wiki
  - https://wiki.haskell.org/Higher_order_function
  - https://wiki.haskell.org/Currying
  - https://wiki.haskell.org/Partial_application
  - https://wiki.haskell.org/Section_of_an_infix_operator
  - https://wiki.haskell.org/Anonymous_function
  - https://wiki.haskell.org/$
      - https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html
  - https://wiki.haskell.org/Function_composition
  - https://wiki.haskell.org/Pointfree
  - https://wiki.haskell.org/Fold
    - https://wiki.haskell.org/Foldr_Foldl_Foldl%27
- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html
- https://en.wikipedia.org/wiki/Tacit_programming