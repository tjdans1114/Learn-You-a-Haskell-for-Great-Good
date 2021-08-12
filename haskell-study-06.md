# I/O

[첫 시간에 봤던 첫 부분](https://github.com/tjdans1114/Learn-You-a-Haskell-for-Great-Good/blob/main/haskell-study-1.md#haskell%EC%9D%B4-%EB%AD%94%EA%B0%80%EC%9A%94)을 잠깐 보고 옵시다

## Hello, world
hello world를 출력해봅시다!!

```bash
ghc --make [filename]
./[filename]
```

```
ghci> :t putStrLn  
putStrLn :: String -> IO ()  
ghci> :t putStrLn "hello, world"  
putStrLn "hello, world" :: IO ()  
```


## Gluing I/O Actions Together
- `main`은 `IO Sth`. `main`이 정해져 있어야 실행 파일 생성 가능.
- `main`이 아닌 다른 `IO ()`도 정의 가능하지만 결국 `main`의 일부가 되어야 의미가 있다


```hs
main = do  -- monad의 syntactic sugar. 모나드끼리 결합..? do로 묶인 전체 모나드 타입 = 맨 마지막 줄 타입
    putStrLn "Hello, what's your name?"  -- 한 줄 한줄이 다 모나드
    name <- getLine  -- 이름 <- expr
    putStrLn ("Hey " ++ name ++ ", you rock!") -- 맨 마지막 줄에서는 <- 로 바인드 불가. expr로 끝나야
```
- IO action 내부에서 `<-`으로 data 꺼내서 이름에 바인드 가능



```hs
nameTag = "sth" ++ getLine -- 왼쪽은 String, 오른쪽은 IO String => invalid
```


```
Prelude> :t return
return :: Monad m => a -> m a
```
- `return`이 프로그램 끝내는 게 아님.. pdf 160쪽 보세여..

## Some Useful I/O functions
- putStr, putChar, print, when, sequence, mapM, mapM_, forever, forM

```
ghci> sequence $ map print [1,2,3,4,5]
Prelude> map print [1, 2,3]

<interactive>:6:1: error:
    • No instance for (Show (IO ())) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
Prelude> let v = map print [1, 2,3]
Prelude> v

<interactive>:8:1: error:
    • No instance for (Show (IO ())) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
Prelude> :t v
v :: [IO ()]
Prelude> :t map
map :: (a -> b) -> [a] -> [b]
Prelude> :t mapM
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
Prelude> sequence v
1
2
3
[(),(),()]
Prelude> :t sequence
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
```
List가 Traversable의 Instance

# More IO
## Files and Streams
- 설명할 내용이 없음. 책을 읽으세요. getContents...?
```
Prelude> :t interact
interact :: (String -> String) -> IO ()
```

## Reading and Writing Files
```hs
import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```

```
Prelude System.IO> :i FilePath
type FilePath = String  -- Defined in ‘GHC.IO’
Prelude System.IO> :i IOMode
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
        -- Defined in ‘GHC.IO.IOMode’
instance Eq IOMode -- Defined in ‘GHC.IO.IOMode’
instance Ord IOMode -- Defined in ‘GHC.IO.IOMode’
instance Show IOMode -- Defined in ‘GHC.IO.IOMode’
instance Read IOMode -- Defined in ‘GHC.IO.IOMode’
instance Enum IOMode -- Defined in ‘GHC.IO.IOMode’

Prelude System.IO> :t hGetContents
hGetContents :: Handle -> IO String

Prelude System.IO> :t hClose
hClose :: Handle -> IO ()
```
Handle 뭔가 file pointer 같은 것.....

```hs
import System.IO     
    
main = do     
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents) 

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result
```
- hGetContents 말고도 hPutStr, hPutStrLn, hGetChar ...
- readFile, wirteFile, appendFile, removeFile, renameFile ...

## TODO List

```hs
import System.IO     
    
main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")  
```

```hs
import System.IO  
import System.Directory  
import Data.List  
  
main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"
```

```hs
import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
            [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")
```

## Command-Line Arguments
```hs
import System.Environment   
import Data.List  
  
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName
```

io-examples에 todo-old, todo파일을 보십시오..


## Randomness
- `System.Random` 모듈에 이것저것이 있어욥
```hs
random :: (Random a, RandomGen g) => g -> (a, g)
mkStdGen :: Int -> StdGen

data StdGen
  = System.Random.StdGen {-# UNPACK #-}GHC.Int.Int32
                         {-# UNPACK #-}GHC.Int.Int32
        -- Defined in ‘System.Random’
instance Show StdGen -- Defined in ‘System.Random’
instance Read StdGen -- Defined in ‘System.Random’
instance RandomGen StdGen -- Defined in ‘System.Random’
```

- RandomGen : 하나의 state. haskell에서 state를 다루는 방식. state는 모나드고... 자세한 설명은 모나드 알려주시는 분이 하실 것

```hs
randoms' :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen  

finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen)

-- random with boundary
randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
randomRs :: (Random a, RandomGen g) => (a, a) -> g -> [a]
```

### Randomness + I/O
```hs
import System.Random  
  
-- generate 2 random str
main = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  -- global generator updated.
    putStr $ take 20 (randomRs ('a','z') gen')     
```

```hs
-- ask for number
import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen

-- newStdGen써서 같은 일 하기
import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)     
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        newStdGen  
        main  
```

## [Bytestrings](https://hackage.haskell.org/package/bytestring)
- [Char]로 파일 읽을 수 있지만 느림. Char는 fixed size 아님
- 큰 파일 읽을 땐 bytestrings 씁시다. fixed size. 1 byte

### Strict Bytestring
- `Data.Bytestring`
- lazy하지 않음. 유한 길이.
- thunk(promise)가 없어서 오버헤드가 적다
- 한 번에 읽어서 메모리 터져요..

### Lazy Bytestring 
- `Data.Bytestring.Lazy`
- lazy한데 []만큼은 아님
- 64K(1 chunk) 단위로 eval.
- 처리할 때 메모리 부하 적음 + 64K가 L2 캐시와 잘 맞음

### 모듈 내부 함수들
- pack, unpack
- fromChunks(strict -> lazy), toChunks(lazy -> strict)
- cons(:)