module NaiveGeometry 
  (sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  , cuboidArea
  , cuboidVolume
  , circleArea
  )  where

rectArea :: Float -> Float -> Float
rectArea a b  =  a * b

circleArea :: Float -> Float
circleArea = (*pi) . (^2) 

sphereVolume :: Float -> Float
sphereVolume r = (4.0/3) * pi * (r^3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r^2)

cubeVolume :: Float -> Float
cubeVolume = (^3)

cubeArea :: Float -> Float
cubeArea = (*6) . (^2) 

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c  = (rectArea a b) * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea s1 s2 s3 = (*2) $ (rectArea s1 s2) + (rectArea s2 s3) + (rectArea s1 s3)
