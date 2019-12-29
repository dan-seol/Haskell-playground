import Data.List
import qualified Data.Map as M -- importing functions from Data.Map that have the same name as the ones in Prelude with differentiated name ("static import")
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

--only importing nub and sort from Data.List
--import Data.List (nub, sort)
--import Data.List hiding (nub)
--
words' :: String -> [String]
words' sentence = case dropWhile isSpace sentence of
                    "" -> []
                    s -> w:words s'
                      where (w, s') = break isSpace s
wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle = any (needle `isPrefixOf`) . tails

encodeCaesar :: Int  -> String -> String
encodeCaesar offset msg = map (\c -> chr $ ord c + offset) msg

decodeCaesar :: Int -> String -> String
decodeCaesar key msg = encodeCaesar (negate key) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]
