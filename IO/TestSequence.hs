main = do 
  -- sequence fucntion takes a list of I/O actions and returns an I/O action
  -- that will perform those axtions one after the other
  rs <- sequence [getLine, getLine, getLine]
  print rs
  -- is equivalent to
  --  a <- getLine
  --  b <- getLine
  --  c <- getLine
  --  print [a, b, c]
  -- map print [1,2,3,4,5] will output [print 1 , print 2, .., print 5]
  -- if you want to print each element, you should
  sequence $ map print [1,2,3,4,5]
  --equivalent to
  mapM print [1,2,3,4,5]
  -- if you want to remove list of units [(),(),(),(),()], use
  mapM_ print [1,2,3,4,5]
