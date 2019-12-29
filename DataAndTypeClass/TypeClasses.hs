data MyTree a = EmptyTree | MyNode a (MyTree a) (MyTree a) deriving (Show)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
 -- Red /= Red = False
  Green == Green = True
 -- Green /= Green = False
  Yellow == Yellow = True
 -- Yellow /= Yellow = False
  _ == _ = False
 -- _ /= _ = True


instance Show TrafficLight where 
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (MyTree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
     then yesResult
     else noResult
