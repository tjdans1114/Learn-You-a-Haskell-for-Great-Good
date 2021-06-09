insertAt c xs n = take (n -1) xs ++ (c : drop (n -1) xs)
