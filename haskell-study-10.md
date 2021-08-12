# Ch.15 Zipper

- 데이터 구조와 그 주변에 초점을 둔 부분을 포함하는 페어를 지퍼라고 한다.

## Tree에 Zipper 이용하기

```haskell
-- 예시에 사용되는 Tree
freeTree :: True Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )
```

- 위에 코드를 그림으로 그림으로 표현한 모습

![tree image](http://learnyouahaskell.com/pollywantsa.png)

### W Tree를 P로 변경하기

**특정 트리로 이동 후 트리 요소를 'P'로 변경하기**
```haskell
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r
```

**특정 트리로 이동 후 트리 요소를 출력하기**
```haskell
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds l
elemAt [] (Node x _ _) = x
```

**example**
```haskell
ghci> let newTree = changeToP [R,L] freeTree
ghci> elemAt [R,L] newTree
'P'
```

### 빵가루 흔적
- 위에 코드를 트리를 변경할 때마다 항상 트리의 루트에서부터 요소를 찾을 때까지 패턴 매칭하는 코드이기 때문에 비효울적이다.
- 트리의 루트에서 이동할 때마다 한 번씩 왼쪽 또는 오른쪽으로 이동하면서 이동경로(Breadcrumbs)를 저장하는 방식으로 수정
  
**이동 경로 남기기**
```haskell
type Breadcrumbs = [Direction]
```
- 이동경로를 남기기 위해서 Breadcrumbs type 선언

**좌우로 이동하기**
```haskell
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)
```

**example**
```haskell
ghci> goLeft (goRight (freeTree, []))
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty), [L,R])

ghci> (freeTree, []) -: goRight -: goLeft
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty) [L,R])
```

- `-:` 함수를 이용해서 좀 더 읽기 쉽게 코드를 작성할 수 있다.

### 트리를 거꾸로 돌아가기
```haskell
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show) 

type Breadcrumbs a = [Crumb a]
```

- 트리를 거꾸로 올라기기 위해서 기존에 `Breadcrumbs` 데이터만으로는 부족해서 새로운 데이터를 추가
- 이동 전 트리 요소와 이동하지않은 반대쪽 Tree를 저장

**goLeft, goRight 수정하기**
```haskell
goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)
```

- 전처럼 좌우로 이동할 수 있으면서 반대쪽 이동경로 노드 및 전 노드 요소도 알게되어서 언제든지 돌아갈 수 있게 되었다.

**거꾸로 올라가기**
```haskell
goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)
```

- 최근 Crumb를 확인해서 Crumb 맞게 전 요소와 이동하지않은 반대쪽 노드, 현재 노드를 이용해서 위로 올라갈 수 있다.


### Zipper
```haskell
type Zipper a = (Tree a, Breadcrumbs a)
```

- 필자는 이와 같은 타입 동의어 이름을 Focus로 하는 것을 선호하지만 이런 과정을 설명하는데 폭넓게 이용되는 이름이 Zipper이기 때문에 그대로 사용한다.

### 초점을 둔 트리 조작하기
```haskell
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)
```

**example**
```haskell
ghci> let newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')

ghci> let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')
```

- newFocus에 기존 이동경로가 저장되어 있어서 newFocus에서도 루트를 이동해서 modify를 동작할 수 있다.


### 새로운 노드 추가하기
```haskell
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)
```

**example**
```haskell
ghci> let farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
ghci> let newFocus = farLeft -: attach (Node 'Z' Empty Empty)
```

### 바로 루트 노드로 이동하기
```haskell
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)
```

## List에 Zipper 적용하기
- List는 단 하나의 하위 트리를 가진 트리로 여길 수 있다 ex) [1,2,3] => 1 -> 2:3:[]
- List는 앞 뒤로만 움직이기 때문에 Tree보다 사용하기 쉽다.
  
**Zipper Type 지정하기**
```haskell
type ListZipper a = ([a], [a])
```

- 페어 듀풀에 첫 번째 인자는 현재 List,
- 페어 듀풀에 두 번째 인자는 이동 경로

**좌우로 이동하기**
```haskell
goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)
```

**example**
```haskell
ghci> let xs = [1,2,3,4]
ghci> goForward (xs, [])
([2,3,4], [1])
ghci> goForward ([2,3,4], [1])
([3,4], [2,1])
ghci> goBack ([3,4], [2,1])
([2,3,4], [1])
```

## File System에 Zipper 적용하기
- 파일 시스템은 주로 파일들과 폴더들로 이루어진다
- 파일은 데이터의 단위이며 이름을 갖는다
- 폴더는 그러한 파일들을 구성하는데 사용되며, 파일이나 다른 폴더를 가질 수 있다.

```haskell
myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]
```

**Zipper Type 지정하기**
```haskell
type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])
```

**계층 구조에서 위로 올라기기**
```haskell
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)
```

** 파일 시스템 안쪽을 들어가기**
```haskell
import Data.List (break)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName
```

- break 
```haskell
ghci Data.List>:t Data.List.break
(a -> Bool) -> [a] -> ([a],[a])

ghci Data.List> Data.List.break (3==) [1,2,3,4,5]
([1,2],[3,4,5])
```


**example**
```haskell
ghci> let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
ghci> fst newFocus
File "skull_man(scary).bmp" "Yikes"
```

**파일 시스템 이름 변경하기**
```haskell
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)
```

**새로운 파일 및 폴더 추가하기**
```haskell
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)
```

## 예외처리하기
- 위에 코드들은 이동으로 인해 잘못되는 경우를 처리하지 않았다.
- 트리에서 empty에서 이동을 하거나 루트 트리에서 올라갈려고하면 런타임에러가 발생할 것이다.


**goLeft goRight**
```haskell
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) - Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) - Just (r, LeftCrumb x l:bs)
goRight (Empty, _) = Nothing
```

**goUp**
```haskell
goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing
```