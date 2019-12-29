main = do
  _ <- putStrLn "It begins"
  foo <- putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
