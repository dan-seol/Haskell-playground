data Race = Human | Orc | Elf | Hafling | Chipmunk
data Profession = Wizard | Warrior | Thief

data Player = Player Race Profession

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Couple b a = Couple {getCouple :: (a, b)}

instance Functor (Couple c) where
  fmap f (Couple (x, y)) = Couple (f x, y)

x = getCouple $ fmap (*100) (Couple (2,3))

data CoolBool = CoolBool {getCoolBool :: Bool}

newtype LazyCoolBool = LazyCoolBool {getLazyCoolBool :: Bool}
-- if helloMe undefined an exception is thrown
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

--if helloLazyMe undefined then returns "hello"
helloLazyMe :: LazyCoolBool -> String
helloLazyMe (LazyCoolBool _) = "hello"

--type String = [Char]; so you can do something like x ++ y
-- if x :: String and y :: [Char]
-- for newtype CharList = CharList { getCharList :: [Char]}
-- if x :: CharList and y :: [Char], then can't do x ++ y
