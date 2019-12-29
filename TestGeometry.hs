import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

testSphereArea :: Float
testSphereArea = Sphere.area 4

testSphereVolume :: Float
testSphereVolume = Sphere.volume 4

testCuboidArea :: Float
testCuboidArea =  Cuboid.area 4 3 2

testCuboidVolume :: Float
testCuboidVolume = Cuboid.volume 4 3 2

testCubeArea :: Float
testCubeArea = Cube.area 4

testCubeVolume :: Float
testCubeVolume = Cube.volume 4 
