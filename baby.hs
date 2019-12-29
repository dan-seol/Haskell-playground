doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 
                         then x 
                         else x*2
doubleSmallNumber' x = (if x>100 then x else x*2) + 1
conanO'Brien = "TeamCoCo"

customList = [1,2,3,4] ++ [9,10,11,12]
cat = 'A':" SMALL CAT" 
--head [5,4,3,2,1] -- 5
--tail [5,4,3,2,1] -- [4,3,2,1]

--last [5,4,3,2,1] -- 1
--init [5,4,3,2,1] --  [5,4,3,2]
--null [1,2,3] -- False
--null [] -- True
--reverse [5,4,3,2,1]
--take 3 [5,4,3,2,1] -- [5,4,3]
--drop 3 [1,2,3,4,5,6] -- [4,5,6]
--drop 100 [1,2,3] -- []
--maximum [1,9,2,3,4] -- 9
--sum [1,2,3] -- 6
--minimum [1,3,5] -- 1
--product [6,2,1,2] -- 24
--elem 4 [1,2,3,4]
--3 `elem` [1,3,5] -- True
--[1..20] -- [1,2,3,4,5,6,7,8,9,10,11,12,13,..,20]
--['a'..'z']
--['K'..'Z']
--[2, 4..20] -- [2,4,6,8..20]
--[3,6..20] -- [3,6,9,12,15,18]
--[13,26..24*13] -- 24 multiples of 13
--take 24 [13,26...] -- does the same
--take 10 (cycle [1,2,3]) -- cycle infinitely repeats a list
--take 12 (cycle "LOL ")
--take 10 (repeat 5) -- repeat infinitely repeats an element
--[x*2 | x <-[1..10]] -- list comprehension!
--[x*2 | x <- [1..10], x*2 >=12]
--[x | x <- [50..100], x `mod` 7 ==3]
boomBangs xs = [if x < 100 then "BOOM" else "BANG!" | x <- xs, odd x]
--[x | x <- [10..20], x /= 13, x /= 15, x /= 19]
--[x + y | x <- [1,2,3], y <- [10,100,1000]]
--[x * y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

--[adj ++ " " ++ noun | adj <- ["lazy", "grouchy", "scheming"], noun <- ["hobo", "frog", "pope"]]

length' xs = sum [1 | _ <- xs]

removeNonUpper st = [c | c <- st, elem c ['A'..'Z']]

--[[1..10], [2, 4..20], [3,6..30]]

--(1,3)
--(1, 'a', "hello")
--fst (2,3)
--snd (4,5)
--zip [1..5] [5,5,5,5,5] -- same as zip [1..5] repeat 5
--zip [1..20] ["Im", "a", "turtle"] -- [(1,"Im"), (2, "a"), (3, "turtle")]
--zip [1..] ["apple", "orange"]
rightTriangles = [(a,b,c)| c<-[1..10], a<-[1..c], b <-[1..a], a^2 + b^2 == c^2]
rightTrianglees' = [(a,b,c)| c<-[1..10], a<-[1..c], b <-[1..a], a^2 + b^2 + c^2 == 24] 
