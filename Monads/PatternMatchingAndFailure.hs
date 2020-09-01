import Control.Monad

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

anotherH = Just "hello" >>= (\(x:xs) -> return x) 

-- bound to fail

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

testTuples1 = [1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch) 

testTuples1' :: [(Int, Char)]
testTuples1' = do
  n <- [1,2]
  ch <- ['a','b']
  return (n, ch)

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x
