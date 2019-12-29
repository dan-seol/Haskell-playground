data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

mikeD :: Person
mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}

adRock :: Person
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}

mca :: Person
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

danS = Person {firstName = "Dan", lastName = "Seol", age = 22}
aanikaR = Person {firstName = "Aanika", lastName = "Rahman", age = 22}

