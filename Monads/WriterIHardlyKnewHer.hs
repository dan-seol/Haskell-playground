import Data.Monoid
import Control.Monad.Writer
naiveIsBigGang :: Int -> Bool
naiveIsBigGang = (>9) 

isBigGang :: Int -> (Bool, String)
isBigGang x = (x>9, "Compared gang size to 9.")

naiveApplyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
naiveApplyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

listApplyLog :: (a, [c]) -> (a ->(b, [c])) -> (b, [c])
listApplyLog (x, log) g = let (y, newLog) = g x in (y, log ++ newLog)

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

-- adding logging to programs
-- implementing euclidean algorithm

myGcd :: Int -> Int -> Int
myGcd a b 
  | b == 0 = a
  | otherwise = myGcd b (a `mod` b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = writer (a, ["Finished with " ++ show a])
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

gcdRev :: Int -> Int -> Writer [String] Int
gcdRev a b
  | b == 0 = writer (a, ["Finished with " ++ show a])
  | otherwise = do
    result <- gcdRev b (a `mod` b)
    tell [show a  ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)

gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcd'' b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result
  
-- Comparing performance between the one with Difference List and the other without
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown n = do
  finalCountDown (n-1)
  tell (toDiffList [show n])

slowFinalCountDown :: Int -> Writer ([String]) ()
slowFinalCountDown n
  | n < 1 = do 
    tell ["0"]
  | otherwise = do 
    slowFinalCountDown (n-1) 
    tell [show n]
