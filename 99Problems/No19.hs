rotate xs n = drop m xs ++ take m xs
  where
    m = rem n (length xs)