import qualified Data.Map as Map
-- Creating type synonyms using type keyword
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

theOtherPhoneBook :: PhoneBook
theOtherPhoneBook = [("Dan", "578-9701"), ("Dan", "259-3810"), ("Aanika", "499-0270")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- type synonyms can also be parametrized
type AssocList k v = [(k, v)]

-- type IntMap v = Map Int v can be written point-free style
type IntMap = Map.Map Int

-- a custom implementation of Either a b
data MyUnion a b = MyLeft a | MyRight b deriving (Eq, Ord, Read, Show)

-- textbook example w.r.t. Guns N'Roses posters and the lockers pg. 131
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> MyUnion String Code
lockerLookup lockerNumber mapping = case Map.lookup lockerNumber mapping of
                                    Nothing -> MyLeft $ "Locker " ++ show lockerNumber ++ " does not exist!"
                                    Just (state, code) -> if state /= Taken
                                                             then MyRight code
                                                             else MyLeft $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- let's test our lockerLookup
lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "ZD39I"))
    ,(101, (Free, "JAH3I"))
    ,(103, (Free, "IQSA9"))
    ,(105, (Free, "QOTSA"))
    ,(109, (Taken, "893JJ"))
    ,(110, (Taken, "99292"))
    ]
