data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , car :: Car
  } deriving (Show)

data Car = Car
  { company :: String
  , model :: String
  , year :: Int
  } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a =
  Vector a
         a
         a
  deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vmult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `scalarMult` (Vector l m n) = Vector (i * l) (j * m) (k * n)
