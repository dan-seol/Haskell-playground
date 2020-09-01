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

coolTree :: Tree Int
coolTree = (Node 1 Empty (Node 3 Empty Empty))
hello = return (coolTree, []) >>= goRight >>= goRight

-- What if you wanted to change your 'W' Node into a 'P' 
-- we can leave "breadcrumbs" as we make updates, so that we won't need to start from the root every time
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Focus a = (Tree a, Breadcrumbs a)

-- helper
(-:) :: a -> (a -> b) -> b
x -: f = f x

goLeft :: Focus a -> Maybe (Focus a)
goLeft (Node x l r, bs) =  Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Focus a -> Maybe (Focus a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: Focus a -> Maybe (Focus a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing 

modify :: (a -> a) -> Focus a -> Focus a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Focus a -> Focus a
attach t (_, bs) = (t, bs)

topMost :: Focus a -> Maybe (Focus a)
topMost (t, []) = Just (t, [])
topMost z = (goUp z) >>= topMost
