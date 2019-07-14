{-# LANGUAGE BangPatterns #-}
-- Haskell version of TCO factorial
fac :: Integer -> Integer
fac n = factt n 1 
    where
        factt :: Integer -> Integer -> Integer
        factt 0 _ = 1
        factt 1 _ = 1
        factt a b = factt (a-1) (a*b)


main :: IO ()
main = print (fac 5)