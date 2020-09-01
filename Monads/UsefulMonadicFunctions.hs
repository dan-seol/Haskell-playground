import Control.Monad.Writer

-- Predicate for the Writer
keepSmall :: Int -> Writer [String] Bool
keepSmall x 
  | x < 4 = do
    tell ["Keeping" ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large. Discarding it away"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

bitSmalls :: Int -> Int -> Maybe Int
bitSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)
