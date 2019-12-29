removeNonUppcase :: [Char] -> [Char]

removeNonUppcase st = [c|c <- st, elem c ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

facHelper :: Int -> Int -> Int
facHelper 0 _ = 1
facHelper 1 b = b
facHelper n1 n2 = facHelper (n1-1) (n1*n2)

fact :: Int -> Int
fact n = facHelper n 1

circumference :: Float -> Float
circumference r = 2 * pi * r

fact2 :: Int -> Int
fact2 n = product [1..n]
