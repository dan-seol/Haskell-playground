solveRPN :: String -> Double
solveRPN [] = error "An empty string is given."
solveRPN expr = head (( foldl calculate [] ) (words expr) )
  where calculate (x:y:ys) "*" = (y * x):ys
        calculate (x:y:ys) "+" = (y + x):ys
        calculate (x:y:ys) "-" = (y - x):ys
        calculate (x:y:ys) "/" = (y / x):ys
        --calculate (x:y:ys) "mod" = (y `mod` x):ys
        calculate (x:y:ys) "^" = (y ** x):ys
        calculate (x:ys) "ln" = (log x):ys
        calculate xs "sum" = [sum xs]
        calculate xs numberString = (read numberString :: Double):xs
