module MyFileUtils 
  ( myWithFile
  ) where

import System.IO
import Control.Exception

myWithFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
myWithFile name mode f = bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)
