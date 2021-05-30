import No09 (pack)

encode xs = [(length x, head x) | x <- pack xs]