import qualified Data.Map as Map
import Data.List
import Data.Char

phoneBook :: [(String, String)]

phoneBook = [("betty", "555-2938")
            ,("bonnie", "452-2928")
            ,("patsy", "493-2928")
            ,("lucille", "202-2928")
            ,("wendy", "939-8282")
            ,("penny", "853-2492")]

findKeyNaive :: (Eq k) => k -> [(k, v)] -> v
findKeyNaive key xs = snd . head . (Data.List.filter) (\(k, v) -> key == k) $ xs

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k, v):xs)
  | key == k = Just v
  | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = Data.List.foldr (\(k, v) -> \acc -> if key == k then Just v else acc) Nothing xs

mapOne = Map.fromList [("MS", 1), ("MS", 2), ("MS", 3)] -- duplicates discarded

phoneBookMapping = Map.fromList phoneBook

phoneNumToDigits :: String -> [Int]
phoneNumToDigits = map digitToInt . filter isDigit

anotherPhoneBook :: [(String, String)]
anotherPhoneBook = [("betty", "555-2938")
                   ,("betty", "342-2492")
                   ,("bonnie", "452-2928")
                   ,("patsy", "493-2928")
                   ,("patsy", "943-2929")
                   ,("patsy", "827-9162")
                   ,("lucille", "205-2928")
                   ,("wendy", "939-8282")
                   ,("penny", "853-2492")
                   ,("penny", "555-2111")]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith append xs
  where append num1 num2 = num1 ++ ", " ++ num2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
