main = do
  putChar 'H'
  putChar 'E'
  putChar 'Y'
  myPutStr ", Anna!"

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do putChar x
                     myPutStr xs
