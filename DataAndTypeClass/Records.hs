data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _ ) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) =  number

favouriteFlavour :: Person -> String
favouriteFlavour (Person _ _ _ _ _ flavour) = flavour

-- Define a type with getters using record syntax
data Student = Student { name :: (String, String)
                       , year :: Int
                       , id :: [Int]
                       , major :: String
                       , cgpa :: Float
                       } deriving (Show)

-- Naive definition of car :data Car = Car String String Int deriving (Show)
-- If you use record syntax, you have more than one way to construct instances of type Car
data Car = Car { company :: String
               , model :: String
               , yearMade :: Int
               } deriving (Show)

mustang :: Car
mustang = Car {company = "Ford", model = "Mustang", yearMade = 1997}

