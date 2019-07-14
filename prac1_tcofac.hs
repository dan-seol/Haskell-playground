-- Strict evaluate to prevent haskell lazy loading thunks
{-# LANGUAGE BangPatterns #-}
-- Haskell version of TCO factorial

facc :: Integer -> Integer -> Integer
facc 0 _ = 1
facc 1 b = b
facc a b = facc (a-1) (a*b)

fac :: Integer -> Integer
fac n = facc n 1

main :: IO()
main = print (fac 10) 