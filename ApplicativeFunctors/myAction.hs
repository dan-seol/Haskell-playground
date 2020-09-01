myNaiveAction :: IO String
myNaiveAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

-- can be replaced with Applicative version
myAction :: IO String
myAction = (++) <$> getLine <*> getLine


main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines concatenated turn out to be: " ++ a
