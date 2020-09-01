type Birds = Int
type Pole = (Birds, Birds)

-- Pierre will carry birds on both sides of his stick while walking on a line.
-- If one of the sides has 4 or more birds than the other, Pierre will fall
-- otherwise will carry on
-- (1, 4) -- ok
-- (5, 2) -- ol
-- (6, 0) -- Pierre will fall

(-:) :: a -> (a -> b) -> b
(-:) x f = f x

--using Monad, take into account cases wherew Pierre will fall 

landLeft :: Birds -> Pole -> Either Pole Pole
landLeft n (left, right) 
  | abs ((left + n) - right) < 4 = Right (left + n, right)
  | otherwise = Left (left + n, right)

landRight :: Birds -> Pole -> Either Pole Pole
landRight n (left, right)  
  | abs ((right + n) - left) < 4 = Right (left, right + n)
  | otherwise = Left (left, right + n)

-- Banana on the wire: Pierre will slip no matter what once he steps on banana
banana :: Pole -> Either Pole Pole
banana x = Left x

-- more with do notation
routine :: Either Pole Pole
routine = do
  start <- return (0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

messedUp :: Either Pole Pole
messedUp = do
  start <- return (0,0)
  first <- landLeft 2 start
  banana first
  second <- landRight 2 first
  landLeft 1 second
