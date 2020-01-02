import System.IO
import MyFileUtils

main = do  
  myWithFile "girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
