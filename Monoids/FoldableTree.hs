import MyTree
import qualified Data.Foldable as F

instance F.Foldable MyTree where
 foldMap f EmptyTree = mempty
 foldMap f (MyNode x l r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)


testTree = MyNode 5 
            (MyNode 3 
              (MyNode 1 EmptyTree EmptyTree) 
              (MyNode 6 EmptyTree EmptyTree)
            ) 
            (MyNode 9 
              (MyNode 8 EmptyTree EmptyTree) 
              (MyNode 10 EmptyTree EmptyTree)
            )
