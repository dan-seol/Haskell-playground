module Geometry.Cube
  ( area
  , volume
  ) where

import qualified Geometry.Cuboid as Cuboid

area :: Float -> Float
area s = Cuboid.area s s s 

volume :: Float -> Float
volume s = Cuboid.volume s s s 
