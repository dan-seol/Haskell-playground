
-- Strict evaluate to prevent haskell lazy loading thunks
{-# LANGUAGE BangPatterns #-}

fact' :: Integer -> Integer
fact' 0 = 1
fact' n = product [1..n]
main :: IO ()
main = print (fact' 10)