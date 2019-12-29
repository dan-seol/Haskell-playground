import Shapes

data IntMaybe = INothing | IJust Int

data StringMaybe = SNothing | SJust String

data ShapeMaybe = ShNothing | ShJust Shape

data Airplane a b c = Airplane { company :: a
                               , model :: b
                               , year :: c       
                               } deriving (Show)

tellAirplane :: (Show a) => Airplane String String a -> String
tellAirplane (Airplane {company = c, model = m, year = y} ) =
  "This " ++ c ++ " " ++ m ++ " was manufactured in " ++ show y

data Vector3D a = Vector3D a a a deriving (Show)

vAdd :: (Num a) => Vector3D a -> Vector3D a -> Vector3D a
(Vector3D i j k) `vAdd` (Vector3D l m n) = Vector3D (i+l) (j+m) (k+n)

vDot :: (Num a) => Vector3D a -> Vector3D a -> a
(Vector3D i1 j1 k1) `vDot` (Vector3D i2 j2 k2) = (i1 * i2) + (j1 * j2) + (k1 * k2)

sMult :: (Num a) => a -> Vector3D a -> Vector3D a
sMult a (Vector3D i j k) = Vector3D (a*i) (a*j) (a*k)
