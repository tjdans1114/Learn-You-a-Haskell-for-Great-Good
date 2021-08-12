# Haskell Study

## Introduction
### Haskell이 뭔가요?
- [순수 함수형 프로그래밍 언어(purely functional programming langugage)](https://en.wikipedia.org/wiki/Purely_functional_programming)
- side effect가 없다고 주장하지만 사실은 그렇지도 않습니다... (side effect가 있는 걸 타입으로 표현할 수 있어요) 
  ```haskell
    ghci> :t print
    print :: Show a => a -> IO ()
    ghci> :t print "Hello, world"
    print "Hello, world" :: IO ()
  ```
  IO 타입이 eval될 때 hello, world를 찍는 사이드 이펙트가 일어나요
- referential transparency(참조 투명성): 똑같은 함수 호출하면 똑같은 결과가 나와요..
- lazy
- 정적 타입 statically typed 그러나 타입 추론 된다

### GHCi 사용하기
- [설치](https://www.haskell.org/platform/) ghc 포함 이것저것 한번에 받습니다
- `:l [파일이름]`으로 hs 파일 로드 가능

## 시작하기
### 연산자들
- 산술 연산자 : `+`, `-` , `*`, `/`, ...
- 논리 연산자 : `&&`, `||`, `not`, `xor`...
- 비교 연산자 : `==`, `/=`, `>=`...

위에서 본 연산자 중 일부는 사실...

### 함수 호출하기
**infix function(중위 함수)** 입니다. 일반적인 함수는 **prefix function(전위 함수)** 입니다. ex) `succ 8`
> 함수는 우선순위가 가장 높음!! 그러니 괄호를 잘 씁시다
>`` ` `` 으로 함수를 싸서 prefix를 infix로 쓸 수 있다고 합니다

### 함수 정의하기
```haskell
add arg1 arg2 = arg1 + arg2
```
> if condition then sth else sth 로 if문도 쓸 수 있는데, else가 필수

>이름 지을 땐 소문자로! 대문자로 시작하면 타입이나 타입클래스입니다

### [List](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/Data-List.html)
```haskell
[1, 2, 3, 4, 5]
[1, 2, 3] ++ [4, 5] --concatenation
 "asdf" == ['a', 's', 'd', 'f'] --c랑 똑같죠..?
 1 : [2, 3] --앞에 넣기
[0, 1, 2] !! 1 --배열 원소 접근
```
#### List 비교
```haskell
ghci> :t [1, 2]
[1, 2] :: Num a => [a] --a에 대해 앞에서부터 해당 연산자로 비교
```

#### 그 외 연산자들
- head, tail, init, last
  ![headtailinitlast](https://user-images.githubusercontent.com/11536816/119012312-e3bbbd80-b9d0-11eb-90dd-1a9bf511c761.png)
- length
- null --is empty?
- reverse ...

### Texas Ranges
*ranges* 로 List를 생성해봅시다!
```hs
Prelude> [5..20]
[5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
Prelude> [2, 4..20]
[2,4,6,8,10,12,14,16,18,20]
Prelude> ['a', 'c'..'z']
"acegikmoqsuwy"

Prelude> :t take
take :: Int -> [a] -> [a]
Prelude> take 3 [1..]
[1,2,3]

Prelude> take 5 (cycle [1,2])
[1,2,1,2,1]
Prelude> take 5 $ cycle [1, 2]
[1,2,1,2,1]
Prelude> take 5 $ repeat 3
[3,3,3,3,3]
Prelude> replicate 5 3
[3,3,3,3,3]
```
### List Comprehension
*Set Comprehension(조건제시법)* 과 비슷
![set comprehension](https://miro.medium.com/max/1400/1*__QkfytLlRx_x3El0C3U7A.png)
<img width="921" alt="Screen Shot 2021-05-24 at 11 38 58 PM" src="https://user-images.githubusercontent.com/11536816/119363878-479ff800-bce9-11eb-86f9-a92395f3e927.png">
```hs
ghci> [x+y | x <- [1,2,3], y <- [10,100,1000]]
[11,101,1001,12,102,1002,13,103,1003]
```
nest해서 쓸 수 있어욥

### [Tuple](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/Data-Tuple.html)
- heterogeneous
- fixed size => vector 나타낼 때 자주 씁니다 (2개 pair, 3개 triple)

#### Pairs
- fst, snd
```hs
Prelude> :t fst
fst :: (a, b) -> a
Prelude> :t snd
snd :: (a, b) -> b
```
- zip
```hs
Prelude> :t zip
zip :: [a] -> [b] -> [(a, b)]
```
## Type
- `:t`를 통해 GHCi에서 타입 확인 가능
-  첫 글자 대문자

#### Common Haskell Types
- Int. c에 int같은 것.. bounded
- Integer : not bounded
- Float(32bit), Double(64bit)
- Bool
- Char ''
- Tuples
#### Type Variables
- 위에서 봤어영..
#### Type Class
interface 같은 것.
A가 타입클래스 Sth의 인스턴스가 되려면 Sth의 minimum complete definition을 구현해야 한다.
```hs
class Sth x where
  f :: x -> Bool

data A = Aaa | Abb deriving (Eq)
-- data (type constructor) = (value constructor)

instance Sth A where -- A should be concrete type(결정되지 않은 게 없다)
  f a = a == Aaa
```

- [Eq](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/Data-Eq.html#t:Eq)
```hs
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool
```
a가 타입클래스Eq의 인스턴스. 이 a가 ==를 지원한다.
- Ord
- Show: string이 될 수 있는?
- Read: string에서 될 수 있는..? 다른 친구랑 추론해서 쓰거나 a를 정해주거나
- Enum (Enumerable), Bounded, Num, Floating, Integral(정수) ...
