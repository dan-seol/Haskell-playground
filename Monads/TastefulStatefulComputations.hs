import System.Random
import Control.Monad.State
-- random generator example
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (first, newGen) = random gen
      (second, newGen') = random newGen
      (third, newGen'') = random newGen'
   in (first, second, third)


type MyStack a = [a]
myPop :: MyStack a -> (a, MyStack a)
myPop (x:xs) = (x, xs)

myPush :: a -> MyStack a -> ((), MyStack a)
myPush a xs = ((), a:xs)

myStackManip :: MyStack Int -> (Int, MyStack Int)
myStackManip stack = let
  ((), newStack1) = myPush 3 stack
  (a, newStack2) = myPop newStack1
  in myPop newStack2

type Stack a = [a]

pop :: State (Stack Int) Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State (Stack Int) ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State (Stack Int) Int
stackManip = do
  push 3
  a <- pop
  pop

stackStuff :: State (Stack Int) ()
stackStuff = do
  a <- pop
  if a == 5
     then push 5
     else do
       push 3
       push 8

moreStack :: State (Stack Int) ()
moreStack = do
  a <- stackManip
  if a == 100
     then stackStuff
     else return ()

stackyStack :: State (Stack Int) ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
     then put [8,3,1]
     else put [9,2,1]

pop' :: State (Stack Int) Int
pop' = do
  xs <- get
  put (tail xs)
  return (head xs)

push' :: Int -> State (Stack Int) ()
push' x = do
  xs <- get
  put (x:xs)

-- Randomness and the State Monad

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

anotherThreeCoins :: State StdGen (Bool, Bool, Bool)
anotherThreeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)
