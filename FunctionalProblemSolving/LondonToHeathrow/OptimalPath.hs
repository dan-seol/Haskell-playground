module OptimalPath
  ( Section (..)
  , RoadSystem
  , Path
  , findOptimalPath
  , testPath
  ) where

data Section = Section Int Int Int deriving (Show)
data CurrentPosition = Above | Below | Crossing deriving (Show)

type RoadSystem = [Section]
type Path = [(CurrentPosition, Int)]

optimalPathHelper :: (Path, Path) -> Section -> (Path, Path)
optimalPathHelper (pathA, pathB) (Section a b c) = 
  let timeAbove = sum (map snd pathA)
      timeBelow = sum (map snd pathB)
      timeAboveCarryOn = timeAbove + a
      timeBelowCarryOn = timeBelow + b
      timeAToB = timeAbove + c + a
      timeBToA = timeBelow + c + b
      pathA' = if timeAboveCarryOn <=
                  timeBToA then (Above, a):pathA else (Crossing, c):(Below, b):pathB
      pathB' = if timeBelowCarryOn <= timeAToB then (Below, b):pathB else (Crossing, c):(Above, a):pathA
   in (pathA', pathB')


findOptimalPath :: RoadSystem -> Path
findOptimalPath roadSystem =
  let (optimalA, optimalB) = foldl optimalPathHelper ([], []) roadSystem
   in if sum (map snd optimalA) <= sum (map snd optimalB)
         then reverse optimalA
         else reverse optimalB

testPath = [ Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
