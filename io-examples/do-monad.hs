main = do  -- monad의 syntactic sugar. 모나드끼리 결합..?
    putStrLn "Hello, what's your name?"  -- 한 줄 한줄이 다 모나드
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")


mymain = putStrLn "Hello" >> getLine >>= \name -> putStrLn ("Hey" ++ name ++ "...")
-- >> : then, >>= : bind
