import System.IO
import Data.Char

main = do
  contents <- readFile "girlfriend.txt"
  putStr contents
  writeFile "girlfriendcaps.txt" (map toUpper contents)
