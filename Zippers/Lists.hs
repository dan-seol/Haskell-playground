data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
type ListFocus a = ([a], [a])

goForward :: ListFocus a -> ListFocus a
goForward (b:xs, bs) = (xs, b:bs)

goBack :: ListFocus a -> ListFocus a
goBack (xs, x:bs) = (x:xs, bs)
