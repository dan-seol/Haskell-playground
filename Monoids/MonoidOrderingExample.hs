import Data.Monoid

lengthCompareNaive :: String -> String -> Ordering
lengthCompareNaive x y = let a = length x `compare` length y
                             b = x `compare` y
                          in if a == EQ then b else a

lengthCompare :: String -> String -> Ordering
lengthCompare x y =  (length x `compare` length y) `mappend` (x `compare` y)

lengthCompareWithVowels :: String -> String -> Ordering
lengthCompareWithVowels x y = (length x `compare` length y) `mappend` ( (vowels x `compare` vowels y) `mappend`( x `compare` y)) where vowels = length . filter ( `elem` "aeiou") 

newtype MyFirst a = MyFirst { getMyFirst :: Maybe a }
  deriving (Eq, Ord, Read, Show)

instance Semigroup (MyFirst a) where
  MyFirst (Just x) <> _ = MyFirst (Just x)
  MyFirst Nothing <> x = x

instance Monoid (MyFirst a) where
  mempty = MyFirst Nothing

