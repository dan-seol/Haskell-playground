multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z 

multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

subFour :: (Num a) => a -> a
subFour = (subtract 4)

churchTwo :: (a -> a) -> a -> a
churchTwo f x = f (f x)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _= []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' f = g
  where g y x = f x y

flip'' :: (a->b->c) -> (b->a->c)
flip'' f x y  = f y x

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x: filter' p xs
  | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] =[]
quicksort' (x:xs) = (quicksort' (filter (<=x) xs)) ++ [x] ++ (quicksort' (filter (> x) xs ))

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

collatzChain :: Int -> [Int]
collatzChain 1 = [1]
collatzChain n
  | even n = n: (collatzChain (n `div` 2))
  | odd n = n: (collatzChain (3*n + 1))

numLongChains :: Int
numLongChains = length (filter isLong (map collatzChain [1..100]))
  where isLong xs  = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map collatzChain [1..100]))

triSum :: Int -> Int -> Int -> Int
-- triSum x y z = x + y + z
-- will be equivalent to
triSum = \x -> \y -> \z -> x + y + z

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList f b l = case l of
                    [] -> error "The list must not be empty"
                    [a] -> (f b a)
                    (a:as) ->  foldlList f (f b a) as

sum' :: (Num a) => [a] -> a
sum' xs = foldlList (\acc x -> acc + x) 0 xs

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList g b m = case m of
                    [] -> error "The list must not be empty"
                    [a] -> (g a b)
                    (a:as) -> foldrList g (g a b) as

apply :: (a -> b) -> [a] -> [b]
apply f xs = foldrList (\x -> \acc -> ((f x) : acc)) [] xs

apply_slow :: (a -> b) -> [a] -> [b]
apply_slow f xs = foldlList (\acc ->  \x -> acc ++ [f x]) [] xs

isMember :: (Eq a) => a -> [a] -> Bool
isMember y ys= foldrList (\x -> \acc -> if x == y then True else acc) False ys

foldl1List :: (a -> a -> a) -> [a] -> a
foldl1List f m = case m of
                   [] -> error "The list must not be empty"
                   [a] -> error "The list cannot be a singleton"
                   (a:as) -> foldlList f a as

foldr1List :: (a -> a -> a) -> [a] -> a
foldr1List g l = case l of
                   [] -> error "The list must not be empty"
                   [a] -> error "The list cannot be a singleton"
                   (b:bs) -> foldrList g b bs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1List max

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc -> \x ->  x : acc) []

revers3 :: [a] -> [a]
revers3 =  foldl (flip (:) ) []

product'' :: (Num a) => [a] -> a
product'' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldrList (\x acc -> if p x then x: acc else acc) []

last'' :: [a] -> a
last'' = foldl1 (\_ -> \x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

scanlListNaive :: (b->a->b) -> b -> [a] -> [b]
scanlListNaive f b l = let scanlListHelper f b l1 = case l1 of 
                                                 [] -> error "The list must not be empty"
                                                 [a] -> [(f b a)]
                                                 (a:as) -> (scanlListHelper f b [a]) ++ (scanlListHelper f (f b a) as)
                   in b:scanlListHelper f b l

scanlList :: (b->a->b)->b->[a]->[b]
scanlList f x [] = [x]
scanlList f x (y:ys) = x : scanlList f (f x y) ys

scanrList :: (a->b->b)->b->[a]->[b]
scanrList f x [] = [x]
scanrList f x (y:ys) = (f y (head partialResult)) : partialResult 
  where partialResult = scanrList f x ys

scanl1List' :: (a->a->a) -> [a] -> [a]
scanl1List' f l 
  | length l < 2 = error "The list must have more than 1 element"
  | otherwise = scanlList f (head l) (tail l)

scanl1List :: (a->a->a) -> [a] -> [a]
scanl1List f [] = error "The list must not be empty"
scanl1List f [a] = error "The list should have more than one element"
scanl1List f (a:as) = scanlList f a as

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1List (+) (map sqrt [1..]))) + 1

--point-free
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

--convert fn x = ceiling (negate (tan (cos (max 50 x))))
fn = (ceiling . negate . tan . cos . max 50)

oddSquareSum :: [Int] -> Int
oddSquareSum = sum . (takeWhile (<10000)) . (filter odd) . (map (^2))
