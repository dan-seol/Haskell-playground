
-- Strict evaluate to prevent haskell lazy loading thunks
{-# LANGUAGE BangPatterns #-}

--Tail recursive factorial
fact' :: Int -> Integer
fact' n = fact'' n 1
          where
            fact'' :: Int -> Int -> Integer
            fact'' 0 _ = 1
            fact'' 1 _ = 1
            fact'' a b = fact'' (a-1) (a*b)
            
            
main :: IO ()
main = print 5