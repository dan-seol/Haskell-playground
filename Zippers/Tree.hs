-- Define the tree data type
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show) 

freeTree :: Tree Char
freeTree =
  Node 'P' 
  (Node 'O' 
  (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) 
  (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))
  ) 
  (Node 'L' 
  (Node 'W' (Node 'C' Empty Empty)(Node 'R' Empty Empty))
  (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))
  )

-- What if you wanted to change your 'W' Node into a 'P' ?
-- Naive implementation
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)
changeToP _ = Empty

-- what about if we make our function take a tree along with a list of Directions
-- will be either L or R, representing left or right, respectively.
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeTo :: Char -> Directions -> Tree Char -> Tree Char
changeTo y (L:ds) (Node x l r) = Node x (changeTo y ds l) r
changeTo y (R:ds) (Node x l r) = Node x l (changeTo y ds r)
changeTo y [] (Node _ l r) = Node y l r

-- a function that gets the element with directions
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x  _ _) = x

-- better, not still not as efficient as it can be
-- after every time we make an update to a subtree, the next update will be done 
-- by starting from the root again and parsing through
-- we can leave "breadcrumbs" as we make updates, so that we won't need to start from the root every time
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type NaiveCrumbs = [Direction]
type Breadcrumbs a = [Crumb a]

naivegoLeft :: (Tree a, NaiveCrumbs) -> (Tree a, NaiveCrumbs)
naivegoLeft (Node _ l _, bs) = (l, L:bs)

naivegoRight :: (Tree a, NaiveCrumbs) -> (Tree a, NaiveCrumbs)
naivegoRight (Node _ _ r, bs) = (r, R:bs)

-- helper
(-:) :: a -> (a -> b) -> b
x -: f = f x

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

type Focus a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Focus a -> Focus a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Focus a -> Focus a
attach t (_, bs) = (t, bs)

topMost :: Focus a -> Focus a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)
