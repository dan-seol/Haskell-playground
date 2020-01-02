import System.Random

dummy :: (Int, StdGen)
dummy = random (mkStdGen 100) :: (Int, StdGen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, genOne) = random gen
      (secondCoin, genTwo) = random genOne
      (thirdCoin, genThree) = random genTwo
  in (firstCoin, secondCoin, thirdCoin)

myRandoms :: (RandomGen g, Random a) => g -> [a]
myRandoms gen = let (value, newGen) = random gen in value:myRandoms newGen

myFiniteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
myFiniteRandoms 0 gen = ([], gen)
myFiniteRandoms n gen = 
  let (value, newGen) = random gen
      (restOfList, finalGen) =  myFiniteRandoms (n-1) newGen
   in (value:restOfList, finalGen)

randomRExample :: (Int, StdGen)
randomRExample = randomR (1,6) (mkStdGen 339353)

randomRsExample :: [Char]
randomRsExample = take 10 $ randomRs ('a', 'z') (mkStdGen 3)


