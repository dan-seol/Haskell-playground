tellFortune :: String -> String
tellFortune = ("You will be a lucky man, " ++)

main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Zis is your future: " ++ (tellFortune name)
