module MyTree 
  ( MyTree (EmptyTree, MyNode)
  , singleton
  , treeInsert
  , treeElem
  ) where

data MyTree a = EmptyTree | MyNode a (MyTree a) (MyTree a) deriving (Show)

singleton :: a -> MyTree a
singleton x = MyNode x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> MyTree a -> MyTree a
treeInsert x EmptyTree = singleton x
treeInsert x (MyNode a left right)
  | x == a = MyNode x left right
  | x < a = MyNode a (treeInsert x left) right
  | x > a = MyNode a left (treeInsert x right)

treeElem :: (Ord a) => a -> MyTree a -> Bool
treeElem x EmptyTree = False
treeElem x (MyNode a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right
