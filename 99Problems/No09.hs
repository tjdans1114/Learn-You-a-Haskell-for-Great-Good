module No09
  ( pack,
  )
where

pack :: [Char] -> [[Char]]
pack = (aux []) . reverse
  where
    aux acc [] = acc
    aux [] (x : xs) = aux [[x]] xs
    aux (accHead@(z : zs) : acc) (x : xs)
      | z == x = aux ((x : accHead) : acc) xs
      | otherwise = aux ([x] : accHead : acc) xs