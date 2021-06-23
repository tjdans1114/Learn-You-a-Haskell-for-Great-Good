

- 하스켈 모듈은 기본적으로 함수, 타입, 타입 클래스를 정의한 파일이다.
- 1~5장에서 다뤘던 모든 함수와 타입, 타입 클래스는 Prelude 모듈이며 디폴트로 임포트된다.



### 모듈 Import

```haskell
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
```

- `import ModuleName` 구문으로 모듈을 import 한다.
- 임포트 구문은 함수를 정의하는 것보다 먼저 있어야한다.



**GHCi 환경에서 모듈을 import하는 방법**

```haskell
ghci> import Data.List
ghci> :m + Data.List
ghci> :m + Data.List Data.Map Data.Set
```

- `:m +`를 사용하면 하번 에 여러 개의 모듈을 `GHCi` 환경에서 import할 수 있다.



**import할 지정하기**

```haskell
import Data.List (nub, sort)
import Data.List hiding (nub)
```



**quailfied import**

- import할 함수 이름이 충돌할 때

```haskell
import Data.Map

test a b = Data.Map.Filter a b
```

```haskell
import qualified Data.Map as M

test a b = M.Filter a b
```

- 이름이 충돌하는 함수를 사용할 때 quailfied 를 사용해서 `import`모듈에 같은 이름에 함수가 있을 때 충돌하는 것을 방지할 수 있다.
- `qualified 모듈이름 as 변경할모듈이름`으로 `qualified import`에 이름을 변경할 수 있다.



### 모듈 함수로 문제 해결하기

**단어 카운팅**

- 해당 문자열에서 문자가 몇 개있는 지 알려주는 함수

```haskell
ghci> wordNums "wa wa wee wa"
[("we", 3), ("wee", 1)]
```

```haskell
import Data.List

wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
wordNums' = map (\ws -> (head ws, length)) (group (sort (words xs)))
```

- word

```haskell
ghci Data.List> words "aa bb aa"
["aa", "bb", "aa"]
```

- Sort

```haskell
ghci Data.List> sort [1,3,2,4]
[1,2,3,4]
```

- group

```haskell
ghci Data.List> group [1,1,3,3,2,3,1]
[[1,1],[3,3],[2],[3],[1]]
```





**건초 더미에서 바늘 찾기**

- 해당 a 값이 b 값에 포함되어 있는 지 확인하는 함수

```haskell
ghci> "art" `isIn` "party"
True
ghci> [1,2] `isIn` [1,3,5]
false
```

```haskell
import Data.List

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
```

- isPrefixOf

```haskell
ghci Data.List> isPrefixOf [1,2,3] [1,2,3,4]
True
ghci Data.List> isPrefixOf [2,3] [1,2,3,4]
False
```

- any

```haskell
ghci> any (1 ==) [0,1,2,3]
True
```





**시저 암호**

- 문자열을 해당 숫자만큼 이동시킨 암호문 return하는 함수

```haskell
ghci> encode 3 "hey mark"
"kh|#pdun"
```

```haskell
import Data.Char

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg
```

- chr

```haskell
ghci Data.Char> chr 97
'a'
```

- ord

```haskell
ghci Data.Char> ord 'a'
97
```



- 암호문을 해독하는 함수

```haskell
ghci> deconde 3 "kh|#pdun"
```

```haskell
import Data.Char
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
```

- negate: 숫자 부호 변경 함수

```haskell
ghci> negate 3
-3
```



**엄격한 레프트 폴드**

```haskell
ghci> foldl (+) 0 (replicate 1000000000 1)
*** Exception: stack overflow
```

```haskell
fold (+) 0 [1,2,3] =
fold (+) (0 + 1) [2,3] =
fold (+) (0 + 1 + 2) [3] =
...
```

- `foldl`에서는 모든 단계에서 실제 누적기를 계산하지않고 메모리에 저장해두기 때문에 스택 오버 플로우를 발생시킨다.

```haskell
ghci Data.List> foldl' (+) 0 (replicate 1000000000 1)
1000000000
```

```haskell
fold' (+) 0 [1,2,3] =
fold' (+) 1 [2,3] =
fold' (+) 3 [3] =
...
```

- `foldl'`에서는 실제 계산을 동작하여 메모리가 쌓이는 일이 없어서 스택 오버 플로우가 발생하지않는다.



**숫자 찾기**

- 숫자로 이루어진 string 값을 더했을 때 40이 되는 최초의 수를 구하는 함수

```haskell
ghci> firstTo40
Just 49999
```



```haskell
import Data.Char

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]
```

- digitToInt

```haskell
ghci Data.Char> digitToInt '3'
3
```



### 값에 키 매핑하기

**어소시에이션 리스트(dictionary)**

- 순서와 상관없이 키/값을 저장하는데 사용되는 리스트

```haskell
phoneBook = 
	[("betty", "555-2938")
	,("bonnie", "452-2928")
	,("patsy", "493-2928")
	,("lucille", "205-8282")
	,("wendy", "939-8282")
	,("penny", "853-2492")
	]
```

```haskell
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
```

- 위에 findKey 함수는 key에 알맞은 페어만 남기고 첫번 째 페어에 값을 return함수다
- 만약 찾고자 하는 키가 없으면 빈 리스트에서 head 값을 얻을려고할 때 런타임 에러가 발생할 것이다.

```haskell
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v -- Maybe는 모나드에서 더 자세하게 알려준다.
findKey key [] = Nothing
findKey key ((k,v):xs)
		| key==x = Just v
		| otherwise = findKey key xs
```

```haskell
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr (\(k, v) acc -> if key === k then Just v else acc) Nothing
```

- 일반적으로, 명시적으로 재귀를 사용하는 것보다 폴드를 이용하는 것이 더 쉽게 읽을 수 있고 구별할 수 있다.



### Data.Map 입력

- `Data.Map` 모듈은 훨씬 빠른 어소시에이션 리스트를 제공하며 유용한 함수를 많이 제공한다.



**formList**

```haskell
ghci> :t Map.formList
Map.formList :: (Ord k) => [(k, v)] -> Map.Map k v
ghci> Map.formList [("MS",1),("MS",2),("MS",3)]
formList [("MS",3)]
```

- 어소시에이션 리스트를 맵으로 변경시킨다
- 원본 어소시에이션 리스트에 동일한 키가 있다면 동일한 것들은 그냥 버려지고 마지막 값만 남는다.
- 기존 어오시에이션 리스트는 키는 동일시 할 수 있기만 하면 되었지만, 이제는 순서를 매길 수 있어야 한다



```haskell
import qualified Data.Map as Map

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
		[("betty", "555-2938")
		,("bonnie", "452-2928")
		,("patsy", "493-2928")
		]
```

- 다른 함수를 테스트하기 위해서 phoneBook이라는 어소시에이션 리스트가 맵이 되도록 작성하였다.



**lookup**

```haskell
ghci> :t Map.lookup
Map.lookup :: (Ord k) => k -> Map.Map k a -> Maybe a
ghci> Map.lookup "betty" phoneBook
Just "555-2938"
ghci> Map.lookup "grace" phoneBook
Nothing
```

- Map에서 특정 키 값을 찾아 찾은 key에 value 값을 반환한다.



**insert**

```haskell
ghci> :t Map.insert
Map.insert :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
ghci> Map.lookup "grace" phoneBook
Nothing
ghci> let newBook = Map.insert "grace" "341-9021" phoneBook
ghci> Map.lookup "grace" newBook
Just "341-9021"
```

- 키, 값, 맵을 받아서 그 키와 값이 삽입되어 있는 새로운 맵을 반환한다



**size**

```haskell
ghci> :t Map.size
Map.size :: Map.Map k a -> Int
ghci Map.size phoneBook
3
ghci> Map.size newBook
4
```

- Map에 length 값을 반환한다



**map**

```haskell
ghci> :t Map.map
Map.map :: (a -> b) -> Map k a -> Map k b
Map.map (++ 1) phoneBook
fromList [("betty","555-29381"),("bonnie","452-29281"),("patsy","493-29281")]
```

- 모든 Map에 함수를 매핑하여 반환한다



**formListWith**

```haskell
phoneBook = [("betty", "555-5555")
						,("betty", "111-2222")
						,("wendy", "555-5555")
						,("penny", "555-5555")]
						
ghci> t: Map.fromListWith
Map.fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
ghci> phoneBookToMap xs = Map.fromListWith add xs where add number1 number2 = number1 ++ ", " ++ number2
ghci> phoneBookToMap phoneBook
fromList [("betty","555-5555, 111-222"),("wendy","555-5555"),("penny", "555-5555")]
```

- 동일한 키를 버리지않고 인자로 받은 함수를 적용시켜 value로 적용시킨다



### 모듈 만들기

- 프로그래밍을 할 때 비슷한 목적을 위해 동작하는 함수들과 타입들을 받아서 별로의 모듈로 두는 것은 좋은 습관이다.
- 모듈은 내부적으로 사용하는 함수를 정의할 수도 있지만, 모듈이 export하는 것들만 볼 수 있으며 사용할 수 있다.



**Geometry 모듈**

```haskell
-- Geometry.hs
module Geomtry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius * 2)

cubeVolume :: Float -> Float
cubeVloume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

rectArea :: Float -> Float -> Float
rectArea a b = a * b
```

- 모듈의 시작 부분에 모듈명을 지정한다
  - 만약에 `Geometry.hs`라는 파일이라면 모듈명도 `Geometry`이어야한다.
- export될 함수를 먼지 지정한 다음에 함수들을 추가할 수 있다.



**계층적인 모듈**

- 각각의 모듈은 수많은 하위 모듈을 가질 수 있으며, 그것들 역시 하위 모듈들을 가질 수 있다.

```haskell
-- Geometry/Sphere.hs
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
vloume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
```

```haskell
-- Geometry/Guboid.hs
module Geometry.Cuboid
( vloume
, area
) where

vloume :: Float -> Float -> Float -> Float
vloume a b c = ractArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectArea a b * 2 + ractArea a c * 2 + ractArea c b * 2

rectArea :: Float -> Float -> Float
rectArea a b = a * b
```

```haskell
-- Geometry/Cube.hs
module Geometry.Cube
( vloume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.vloume side side side

area :: Float -> Float
area side = Cuboid.area side side side
```

- 각 모듈들을 Geometry에 넣고 모듈명을 `Geometry.FileName` 으로 설정한 것을 주목하자

```haskell
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
```

- 크고 엄청난 양의 함수들을 가진 파을 작성할 때는 일반적으로 사용하는 함수들을 찾아서 모듈로 만드는 것을 고려해보자