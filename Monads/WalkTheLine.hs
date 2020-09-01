type Birds = Int
type Pole = (Birds, Birds)

-- Pierre will carry birds on both sides of his stick while walking on a line.
-- If one of the sides has 4 or more birds than the other, Pierre will fall
-- otherwise will carry on
-- (1, 4) -- ok
-- (5, 2) -- ol
-- (6, 0) -- Pierre will fall

naiveLandLeft :: Birds -> Pole -> Pole
naiveLandLeft n (l, r) = (l+n, r)

naiveLandRight :: Birds -> Pole -> Pole
naiveLandRight n (l, r) = (l, r+n)

(-:) :: a -> (a -> b) -> b
(-:) x f = f x

--using Monad, take into account cases wherew Pierre will fall 

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) 
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)  
  | abs ((right + n) - left) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- Banana on the wire: Pierre will slip no matter what once he steps on banana
banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

doFoo :: Maybe String
doFoo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

-- more with do notation
routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

messedUp :: Maybe Pole
messedUp = do
  start <- return (0,0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second
