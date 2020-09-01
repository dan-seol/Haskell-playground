(<^>) :: [a -> b] -> [a] ->  [b]
(<^>) [] _ = []
(<^>) _ [] = []
(<^>) (f:fs) (x:xs) 
  | (length fs) /= (length xs) = error "Please give lists of matching lengths"
  | otherwise = (f x):(fs <^> xs)
