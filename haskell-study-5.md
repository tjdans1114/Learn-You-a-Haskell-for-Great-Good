# Making Our Own Types and Typeclasses 

## Algebraic Data Types (대수적 데이터 구조)
- Prelude 와 같은 표준 라이브러리에 정의되어있는 타입을 사용할 수 있지만, 
- 나만의 타입을 정의하려면? (나만의 리스트/트리/맵 ...)
    - "data" 키워드 사용
```haskell
data Bool = False | True

data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
```

### Shape 예제
- Shape 타입: 원 (Circle) 또는 직사각형 (Rectangle) 
- 원을 나타내려면?
    - 튜플?  (43.1, 55.0, 10.4)
```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)
```

- Circle과 Rectangle은 값 생성자 (Data Constructor)라고 함.
- 값 생성자는 필드들을 매개변수로 받아서 타입을 반환하는 함수.

```haskell
ghci> :t Circle  
Circle :: Float -> Float -> Float -> Shape  
ghci> :t Rectangle  
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

- 따라서 부분적용 후 매핑 가능.
```haskell
ghci> map (Circle 10 20) [4,5,6,6]  
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
``` 

- Shape을 받아서 면적을 반환하는 함수
```haskell
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

- 여전히 모호한 표현을 개선하려면? Point 타입을 도입해보자.
```haskell
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

- 면적을 구하는 surface 함수도 변경 필요.
```haskell
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

- 도형을 x 축/y 축 원하는 만큼 이동하기.
```haskell
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a  b = ???
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = ???
```

- 기본 도형: (0, 0) 을 기준으로 생성.
```haskell 
baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)
```

- nudge + base{Circle, Rect} 조합으로 원하는 위치의 도형을 생성할 수 있음.

### Record Syntax (레코드 구문)
- 데이터 생성자가 여러 가지 필드들을 가지고 있으며, 그들 간의 구분을 명시적으로 할 수 있게 함. 
- 데이터베이스의 Row 또는 레코드. 
```haskell
data Person = Person String String Int Float String String deriving (Show)

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)
```

- 첫번째 Person은 특정 필드값을 가져오려면 getter 함수들을 따로 정의해야함.
```haskell
firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname

(...)
```

- 두 번째 Person은 기본으로 지원.
```haskell
ghci> :t flavor  
flavor :: Person -> String  
ghci> :t firstName  
firstName :: Person -> String
```

- 레코드 구문으로 정의된 데이터를 생성할 때 필드의 순서를 지키지 않고 만들 수 있음.
```haskell
data Car = Car String String Int deriving (Show) 
Car "Ford" "Mustang" 1967

vs.

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  
Car {company="Ford", model="Mustang", year=1967} 
```

### 타입 매개변수 
- 제네릭 타입 선언: 구체적인 타입이 아닌 타입 생성자
```haskell
data Maybe a = Nothing | Just a 
```
- Maybe: 타입 생성자
- Maybe Int, Maybe Float, Maybe Car, Maybe Shape, ... : 타입

```haskell
ghci> Just "Haha"  
Just "Haha"  
ghci> Just 84  
Just 84  
ghci> :t Just "Haha"  
Just "Haha" :: Maybe [Char]  
ghci> :t Just 84  
Just 84 :: (Num t) => Maybe t  
ghci> :t Nothing  
Nothing :: Maybe a  
ghci> Just 10 :: Maybe Double  
Just 10.0
```

- 타입 매개변수는 필요한 곳에만 사용하면 좋음.
```haskell
data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show)

vs.

data Car a b c = Car { company :: a  
      , model :: b  
      , year :: c   
      } deriving (Show)
```
- 리스트와 같이 구체적인 타입이 작업하는데 중요하지 않을 때 사용.
 
- Vector 예제.
```haskell
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n 
```
    - (Num a) => 타입 제약 (Constraint)를 줌으로써 Vector Int, Vector Float, Vector Integer 등을 모두 지원.
    - Vector Bool, Vector Char 등 타입 매개변수가 (Num a) 와 관련 없으면 (타입 인스턴스가 아니면)
        - vplus, vectMult, scalarMult 동작하지 않음.

### 파생된 인스턴스 (Derived Instances) 
- 타입 클래스는 어떤 동작을 정의하는 인터페이스의 일종
- 타입 클래스의 동작을 지원한다면 그 타입은 타입 클래스의 인스턴스
- 예, Int 타입은 Eq 타입 클래스의 인스턴스 (`==`, `/=` 함수를 구현)

- OOP class 와는 다른 개념. (오브젝트를 생성하기 위함이 아닌 공통된 동작을 정의)

```haskell
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)

data Bool = False | True deriving (Ord)
```

- Eq: `==`, `/=`
- Show: `show`
- Read: `read`
- Ord: `compare`
 
```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

ghci> Wednesday  
Wednesday  
ghci> show Wednesday  
"Wednesday"  
ghci> read "Saturday" :: Day  
Saturday  

(Eq, Ord)
ghci> Saturday == Sunday  
False  
ghci> Saturday == Saturday  
True  
ghci> Saturday > Friday  
True  
ghci> Monday `compare` Wednesday  
LT  

(Bound)
ghci> minBound :: Day  
Monday  
ghci> maxBound :: Day  
Sunday 

(Enum)
ghci> succ Monday  
Tuesday  
ghci> pred Saturday  
Friday  
ghci> [Thursday .. Sunday]  
[Thursday,Friday,Saturday,Sunday]  
ghci> [minBound .. maxBound] :: [Day]  
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]  
```

### 타입 동의어 (Type synonyms)
- 한 타입에 다른 이름을 주는 것.
- 코드의 문서화에 유리.
```haskell
type String = [Char]
```

- 보기 좋은 전화번호부
```haskell
phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]
```

```haskell
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook 
```

- 타입 동의어 매개변수화하기
```haskell
type AssocList k v = [(k,v)]

(부분 적용)
type IntAssocList = AssocList Int
```

### Either, Left, Right
- 매개변수로 두 개의 타입을 받는 데이터 타입 중 한 가지.
```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

```haskell
(사용예)
ghci> Right 20  
Right 20  
ghci> Left "w00t"  
Left "w00t"  
ghci> :t Right 'a'  
Right 'a' :: Either a Char  
ghci> :t Left True  
Left True :: Either Bool b
```

- 계산 중 오류가 발생함을 나타내는 방법으로 Maybe 대신 사용 가능.
    - Nothing 은 구체적인 에러 상황에 대한 정보를 알 수 없음.
```haskell
import qualified Data.Map as Map  
  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
```

## 재귀적인 데이터 구조
- 필드의 타입으로 자기 자신을 갖는 타입.
- 대표적으로 리스트 타입.

### 리스트 타입
```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord) 

(또는 레코드 타입으로,)

data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
```

- `Cons`는 `:`의 다른 표현 (사용자가 지정한)
```haskell
ghci> Empty  
Empty  
ghci> 5 `Cons` Empty  
Cons 5 Empty  
ghci> 4 `Cons` (5 `Cons` Empty)  
Cons 4 (Cons 5 Empty)  
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))  
Cons 3 (Cons 4 (Cons 5 Empty))
```

- 특수기호를 사용하여 정의할 수도 있음.
```haskell
infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  
```

- 여기서 `:-:`는 중위 연산자 이면서 우선순위가 5 (fixity declaration)
- fixity declaration은 필수가 아님.
```haskell
ghci> 3 :-: 4 :-: 5 :-: Empty  
(:-:) 3 ((:-:) 4 ((:-:) 5 Empty))  
ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
ghci> 100 :-: a  
(:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))
```

- 리스트 Concat 을 위한 사용자 정의 함수.
```haskell
infixr 5  ^++  
(^++) :: List a -> List a -> List a   
Empty ^++ ys = ys  
(x :-: xs) ^++ ys = x :-: (xs ^++ ys) 
```

### 트리 타입
- 이진 트리 (Binary tree):
    - 각 노드는 최대 2개의 subtree를 가지고,
    - 왼쪽 subtree는 현재 노드의 값보다 작은 값,
    - 오른쪽 subtree는 현재 노드의 값보다 큰 값을 가지는 노드들로 구성됨.

```haskell
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

- 노드 생성 함수 (singleton) 와 삽입 함수 (treeInsert) 
```haskell
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)
```

- element 인지 검사하는 함수.
```haskell
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right
```

- 리스트로부터 트리 생성하기
```haskell
ghci> let nums = [8,6,4,1,7,3,5]  
ghci> let numsTree = foldr treeInsert EmptyTree nums  
ghci> numsTree  
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
```

## 타입 클래스 102
- 사용자 커스텀 타입 클래스 만들기.
- 타입이 타입 클래스의 인스턴스라고 할 때는 그 타입이 타입 클래스의 함수를 구현하였다는 의미.

### Eq 타입 클래스
```haskell
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)
```
- 타입 클래스의 함수를 구현하는 것은 필수 아님.
- 함수 시그니처는 필수.

#### 신호등 데이터 타입
```haskell
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False
    
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"
    
    
ghci> Red == Red  
True  
ghci> Red == Yellow  
False  
ghci> Red `elem` [Red, Yellow, Green]  
True  
ghci> [Red, Yellow, Green]  
[Red light,Yellow light,Green light]
```

### 매개변수화된 타입과 타입 클래스 인스턴스
```haskell
instance Eq Maybe where  
    ... 
```

- Maybe: 타입 생성자 (type constructor)
- Maybe Int: 타입

```haskell
instance Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False
```

- 그런데, 타입 매개변수 m에 대해 `==` 이 지원되는지 알수 없음.
- Constraint: (Eq m) =>
```haskell
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False 
```

## Functor 타입 클래스
- 매핑될 수 있는 것들을 위한 무엇...
- 이때까지 배운 List 도 Functor 타입 클래스 인스턴스

- 정의
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

- list map 함수와의 비교
```haskell
map :: (a -> b) -> [a] -> [b]
```

- Functor의 f 는 구체적인 타입이 아닌 타입 생성자!
- 하나의 타입을 받아서 구체적 타입을 반환
    - Maybe
    - Either?
 
### Functor Maybe

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing
```

- 적용 예
```haskell
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  
Just "Something serious. HEY GUYS IM INSIDE THE JUST"  
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing  
Nothing  
ghci> fmap (*2) (Just 200)  
Just 400  
ghci> fmap (*2) Nothing  
Nothing  
```

### Functor Tree
```haskell
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
```

- 적용 예
```
ghci> fmap (*2) EmptyTree  
EmptyTree  
ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])  
Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree
```

### Functor Either a

- Either 는 두개의 타입 매개변수를 받음.
- Functor는 한개의 타입 매개변수를 받음.
- Either a 는 한개의 타입 매개변수를 받음. 

- 따라서 `Functor Either` 가 아닌 `Functor Either a`

```haskell
data Either a b = Left a | Right b 

instance Functor (Either a) where  
    fmap f (Right x) = Right (f x)  
    fmap f (Left x) = Left x
```
