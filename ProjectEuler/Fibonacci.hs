tailFibo :: Integer -> Integer
tailFibo n = (fibohelper n 1 2)
  where fibohelper 0 a b = a
        fibohelper 1 a b = b
        fibohelper n a b = fibohelper (n-1) b (a+b)

evenHelper :: Integer -> Integer -> Integer -> Integer
evenHelper 0 a b  = a
evenHelper 1 a b = b
evenHelper n a b = evenHelper (n-1) b (a+4*b)



evenFibo :: Integer -> Integer
evenFibo n = (evenHelper n 0 2)
fromOne :: [Integer]
fromOne = [1..]

y :: [Integer]
y = takeWhile (<=4000000) (map evenFibo fromOne)

w :: [Integer]
w = takeWhile (<=8) y

main = putStrLn (show (sum y))