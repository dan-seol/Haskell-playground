import Control.Monad

main = do
  input <- getLine
  
  when (input == "SWORDFISH") $ do 
    putStrLn input 
  -- the block above is equivalent to
  -- if (input == "SWORDFISH")
  -- then  putStrLn input
  -- else return ()
  main
