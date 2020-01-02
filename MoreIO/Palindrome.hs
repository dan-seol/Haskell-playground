module Palindrome 
  ( respondPalindromes               
  , isPalindrome
  ) where
  isPalindrome :: String -> Bool
  isPalindrome xs = xs == reverse xs
                    
  respondPalindromes :: String -> String
  respondPalindromes = unlines .map 
                        (\xs -> if isPalindrome xs then "palindrome" 
                        else "not a palindrome")
                        . lines

