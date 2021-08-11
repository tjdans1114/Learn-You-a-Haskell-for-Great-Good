# Monoid
- 집합 `M` 와 그 집합의 이항 연산(binary operation) `* : M x M -> M` 가 다음 조건을 만족하면
    1. 결합법칙(associativity) : 모든 `M` 의 원소 `a`, `b`, `c` 에 대해 `(a * b) * c = a * (b * c)`
    2. 항등원(identity) : `M` 의 어떤 원소 `e` 가, 모든 M의 원소 `a` 에 대해 `a * e = e * a = a` 
- `(M, *, e)` 를 monoid라고 한다

## Monoid type class

```haskell
class Semigroup m => Monoid m where
  mempty :: m -- identity of monoid

  -- (<>) :: m -> m -> m
  -- operation of monoid (inherited from Semigroup)
  -- `mappend` is synonym for (<>)

  mconcat :: [m] -> m
  mconcat = foldr mappend mempty  
```

## Monoid Examples

- 더 많은 예시는 reference 참조

### List Monoid

```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

## Number monoid

- `(Num, +, 0)` 은 monoid
- `(Num, *, 1)` 은 monoid

## Boolean monoid

- `(Bool, &&, True)` 는 monoid
- `(Bool, ||, False)` 는 monoid

## Ordering monoid

```haskell
instance Monoid Ordering where
  mempty = EQ

  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT
```

## Maybe Monoid

```haskell
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
 
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

## `newtype` : 기존 type을 wrapping하는 방법

- `(Num, +, 0)` 과 `(Num, *, 1)` 이 모두 monoid라면 이 둘을 어떻게 구분하는가?
- `newtype` keyword를 통한 type wrapping을 이용

```haskell
newtype Product a = Product { getProduct :: a }

instance Num a => Monoid (Product a) where
  mempty = Product 1
  
  Product x `mappend` Product y = Product (x * y)

-- 

newtype Sum a = Sum { getSum :: a }

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  
  Sum x `mappend` Sum y = Sum (x + y)
```
- `(Bool, &&, True)`, `(Bool, ||, False)`도 각각 `All`, `Any` monoid로 존재

### `type` vs `data` vs `newtype`

- `data` : algebraic data type을 만들 때 사용
- `type` : type alias를 정의할 때 사용
- `newtype` : type wrapping에 사용. constructor를 정확히 하나만 가지는 `data` declaration으로 간주할 수 있다.

### laziness of `newtype`

- `data` 와 다르게, pattern matching 시에 value가 실제로 사용되지 않는다면 '상자'를 까보지 않음
- 내부적으로는 `newtype` 과 `data` 의 동작 방식이 다르다는 것을 의미

## Monoid로 폴드하기

- `Foldable`  instance를 위해서 `foldMap` 을 구현하자

```haskell
foldMap :: Foldable t => Monoid m => (a -> m) -> t a -> m
```

- `Tree` 에 대한 fold 구현

```haskell
import qualified Data.Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a)

instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = 
    F.foldMap f l `mappend` f x `mappend` F.foldMap f r
```

## References

- Monoid
    - https://en.wikipedia.org/wiki/Monoid
    - https://wiki.haskell.org/Monoid
- Fold / Foldable
    - https://wiki.haskell.org/Fold
    - https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Foldable.html
- newtype
    - https://wiki.haskell.org/Newtype
    - https://wiki.haskell.org/Type


# Monad
> *"A monad is a monoid in the category of endofunctors, what's the problem?"* - Philip Wadler

## Monad : 강화된 어플리커티브 펑터

- Functor : `a -> b` 를 `f a` 에 적용하기
- Applicative : `f (a -> b)` 를 `f a` 에 적용하기
- Monad : `a -> f b` 를  `f a` 에 적용하기

## 모나드 타입 클래스

```haskell
class Applicative m => Monad m where
  return :: a -> m a
  return = pure

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
```

- `return` : value를 default context에 넣는 작업. `unit` 이라고도 부름
- `(>>=)` : 함수 application에 해당함. `bind` 라고도 부름
    - monad context 안에 있는 값에 대해 apply한다고 생각하자.

## Maybe Monad

```haskell
instance Monad Maybe where
  return x = Just x

  Nothing >>= f = Nothing
  Just x >>= f = f x
  
  fail _ = Nothing

-- 
Prelude> Just "smile" >>= \x -> Just (x ++ " :)")
Just "smile :)"
Prelude> Nothing >>= \x -> Just (x ++ " :)")
Nothing
Prelude> Just 3 >>= \x -> if x > 2 then Just x else Nothing
Just 3
Prelude> Just 1 >>= \x -> if x > 2 then Just x else Nothing
Nothing
```

## Example : 줄 타기

```haskell
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left+n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

--

Prelude> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight(-2)
Nothing

Prelude> return (0,0) >>= landLeft 1 >> Nothing
Nothing
```

### Monad가 없다면?

```haskell
routine :: Maybe Pole
routine = case landLeft 1 (0,0) of 
  Nothing -> Nothing
  Just pole1 -> case landRight 4 pole1 of 
    Nothing -> Nothing
    Just pole2 -> case landLeft 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft 1 pole3

-- 
routine2 = return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft 2 >>= landLeft 1
```

## do 표기법

- monadic expression의 syntactic sugar

```haskell
thing1 >>= (\x -> func1 x >>= (\y -> thing2 
       >>= (\_ -> func2 y >>= (\z -> return z))))
-- equals

thing1  >>= \x ->
func1 x >>= \y ->
thing2  >>= \_ ->
func2 y >>= \z ->
return z

-- equals

do {
  x <- thing1 ;
  y <- func1 x ;
  thing2 ;
  z <- func2 y ;
  return z
}
```

- do notation으로 작성한 코드는 컴파일러가 자동으로 `>>=` , `>>` 로 바꿔줌

### do 표기법 : 줄타기

```haskell
routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second
```

### `fail` 함수

- `fail` : `do` notation 내부의 pattern matching에 실패했다거나 하는 경우 사용됨
- `MonadFail` class 로 분리되도록 바뀜
- 프로그램 전체에 대한 exception을 사용하는 대신 monad context 단위에서 fail 하는 방법을 제공함

```haskell
do
  (x:xs) <- Just ""
  return x
-- yields "Nothing"
```

## 리스트 모나드

```haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _ = []
```

### do notation & list comprehension

```haskell
do
  n <- [1,2]
  ch <- ['a', 'b']
  return (n, ch)
-- equals
[(n,ch) | n <- [1,2], ch <- ['a','b']]
```

- guarding은 어떻게? : list map 단계에서 empty list를 반환하도록 만들면 concat할 대상이 없어지니 filter가 되는 효과.

    ```haskell
    -- Alternative : A monoid on applicative functors
    guard :: Alternative f => Bool -> f ()
    guard True  = pure ()
    guard False = empty -- identity of the monoid

    --

    do
      x <- [1 .. 50]
      guard ('7' `elem` show x)
      return x
    -- equals
    [x | x <- [1 .. 50], '7' `elem` show x]

    ```

## 모나드 규칙

1. left identity : `return a >>= f = f a` 
2. right identity : `m >>= return = m`
3. associativity : `(m >>= g) >>= h = m >== (\x -> g x >>= h)`

### `<=<` : Klesli composition operator

```haskell
-- import Control.Monad

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
f <=< g = \x -> g x >>= f

(>=>):: Monad m => (a -> m b)-> (b -> m c)-> (a -> m c)
f >=> g = \x -> f x >>= g
```

1. left identity : `return <=< f = f`
2. right identity : `f <=< return = f`
3. associativity : `(f <=< g) <=< h = f <=< (g <=< h)`
- 즉, `(MonadicFunctions, <=<, return)` 은 monoid가 된다
    - `MonadicFunctions` 는 `a -> m b` 인 함수들을 뜻함

### `do` notation과 모나드 규칙

1. Left identity

    ```haskell
    do { x′ <- return x; f x′}
    -- equals
    do { f x }
    ```

2. Right identity

    ```haskell
    do { x <- m; return x }
    -- equals
    do { m }
    ```

3. Associativity

    ```haskell
    do { y <- do {
           x <- m;
           f x
         }
         g y
       }
    -- equals
    do { x <- m;
         do { y <- f x;
              g y
            }
       }
    -- equals
    do { x <- m;
         y <- f x;
         g y
       }
    ```

## Summary

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  -- (fmap f xs) can be considered as
  -- do { x <- xs ; pure (f x) }

class Functor f => Applicative f where
  pure :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b
  -- (fs <*> xs) can be considered as
  -- do { f <- fs ; x <- xs ; pure (f x) }
  
class Semigroup a => Monoid a where
  mempty  :: a

  mappend :: a -> a -> a
  mappend = (<>)

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  -- (as >>= bs) can be considered as
  -- do { a <- as ; bs a }

  return :: a -> m a
  return = pure
```

## References

- Monad
    - https://wiki.haskell.org/Monad
    - https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html
- do notations
    - https://wiki.haskell.org/Monad#do-notation
    - https://en.wikibooks.org/wiki/Haskell/do_notation
    - https://wiki.haskell.org/Monads_as_computation#Do_notation
- Monad laws
    - https://wiki.haskell.org/Monad_laws
- understanding monads
    - https://www.reddit.com/r/math/comments/ap25mr/a_monad_is_a_monoid_in_the_category_of/
    - https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem
- https://wiki.haskell.org/List_comprehension