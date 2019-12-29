fibtail :: Int -> Int
fibtail n = (fibhelper n 0 1) 
  where fibhelper 0 a b = a
        fibhelper 1 a b = b
        fibhelper n a b = fibhelper (n-1) b (a+b)

maximum' :: (Ord a) => [a] -> a
maximum' l = case l of
               [] -> error "maximum of empty list"
               (x:[]) -> x
               (x:xs) -> max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x 
  | n <= 0 = []
  | otherwise = x: replicate' (n-1) x

reptail :: Int -> a -> [a]
reptail n x = (rephelper n x []) 
  where rephelper :: Int -> a -> [a] -> [a] 
        rephelper 0 x l = l
        rephelper n x l = rephelper (n-1) x (x:l)

taketail :: (Num i, Ord i) => i -> [a] -> [a]
taketail n l1 = takehelper n l1 [] 
  where takehelper :: (Num i, Ord i) => i -> [a] -> [a] -> [a]
        takehelper n x y
          | n <= 0 = y
        takehelper _ [] y = y
        takehelper n (x:xs) y = takehelper (n-1) xs (y ++ [x])

reversetail :: [a] -> [a]
reversetail l = reversehelper l []
  where reversehelper :: [a] -> [a] -> [a]
        reversehelper [] y = y
        reversehelper (x:xs) y = reversehelper xs (x:y)

repeat' :: a -> [a]
repeat' a = a:(repeat' a)

ziptail :: [a] -> [b] -> [(a,b)]
ziptail x' y' = ziphelper x' y' []
  where ziphelper :: [a] -> [b] -> [(a,b)] -> [(a,b)]
        ziphelper [] _ z' = z'
        ziphelper _ [] z' = z'
        ziphelper (x:xs) (y:ys) z = ziphelper xs ys (z ++ [(x,y)])
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let l = [a | a <- xs, a <= x]
      r = [b | b <- xs, b > x]
   in quicksort l ++ [x] ++ quicksort r
