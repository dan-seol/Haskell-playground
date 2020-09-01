±:wjackpot :: Int -> Int -> Int -> String
jackpot 7 7 7 = "DING DING DING DING !"
jackpot x y z = "Sorry, you're out of luck, mate!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe _ = "Should be between 1 and 5"

charMap :: Char -> String
charMap 'a' = "Aanika"
charMap 'd' = "Dan"
charMap 's' = "Sueyeun"
