jackpot :: Int -> Int -> Int -> String
jackpot 7 7 7 = "DING DING DING DING !"
jackpot x y z = "Sorry, you're out of luck, mate!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe _ = "Should be between 1 and 5"

charMap :: Char -> String
charMap 'a' = "Aanika"
charMap 'd' = "Dan"
charMap 's' = "Sueyeun"

vectorSum :: (Double, Double) -> (Double, Double) -> (Double, Double)
vectorSum (x1, y1) (x2, y2) = (x1+x2, y1+y2)

fstTri (x, _, _) = x
sndTri (_, y, _) = y
trdTri (_, _, z) = z

sndElem :: [a] -> a
sndElem [] = error "Empty list, heh"
sndElem (x:[]) = error "We're looking for the element right after the head"
sndElem (x:y:_) = y

verboseShow :: (Show a) => [a] -> String
verboseShow [] = "The list is empty"
verboseShow (x:[]) = "The list is a singleton: " ++ show x
verboseShow (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
verboseShow (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z

firstChar :: String -> String
firstChar "" = "Empty string, whoopsy!"
firstChar all@(x:xs) = "The first letter of  " ++ all ++ " is " ++ [x]

-- Guards

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, eat more!"
  | bmi <= 25.0 = "Looking good!"
  | bmi <= 30.0 = "You're overweight. Let's work out together"
  | otherwise = "You're obese. Go see a doctor."

bmiTell' :: Double -> Double -> String
bmiTell' weight height = bmiTell (weight / height ^ 2)

max' :: (Ord a) => a -> a -> a
max' a b 
  | a <= b = b
  | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b 
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

-- functions with guards and where binding

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
  | bmi <= skinny = "You're underweight, eat more!"
  | bmi <= normal = "Looking good!"
  | bmi <= overweight = "You're overweight. Let's work out together!"
  | otherwise = "You're obese. Go see a doctor."
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        overweight = 30.0

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

-- functions with let expressions

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: [(Double, Double)] -> [Double]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- case expressions
--head' :: [a] -> a
--head' [] = error "It's empty!"
--head` (x:_) = x
-- is equivalent to

head'' :: [a] -> a
head'' xs = case xs of 
              [] -> error "It's empty!" 
              (x:_) -> x

-- function pattern matching - can be only used when defining functions
-- case expressions - can be used anywhere like in the middle of an expression

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of
                                      [] -> "empty"
                                      [x] -> "a singleton"
                                      xs -> "a longer list"

-- equivalently

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
  where what [] = "empty"
        what [x] = "a singleton"
        what xs = "a longer list"
