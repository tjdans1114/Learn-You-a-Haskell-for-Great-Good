dropEvery xs n = [x | (i, x) <- zip [1 ..] xs, mod i n /= 0]