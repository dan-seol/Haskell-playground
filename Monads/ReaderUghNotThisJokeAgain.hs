import Control.Monad.Instances

-- Reader Monad
addStuff :: Int -> Int
addStuff = (*2) >>= (\w -> (+(10+w)))

doAddStuff :: Int -> Int
doAddStuff = do
  a <- (*2)
  b <- (+10)
  return (a + b)
