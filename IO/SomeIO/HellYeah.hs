main = do
  let a = "hell" -- equivalent to doing a <- return "hell"
      b = "yeah" -- b <- return "yeah"
  putStrLn $ a ++ " " ++ b
