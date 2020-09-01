import MyTree
instance Functor MyTree where
  fmap f EmptyTree = EmptyTree
  fmap f (MyNode x left right) = MyNode (f x) (fmap f left) (fmap f right)
