data Thing = Shoe
           | Ship
           | SealingWax
           | King
           | Cabbage
  deriving Show

data Pair = I Int
          | D Double

data Pair2 = Int
           | Double

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

data FailableDouble = Failure
                    | OK Double Int
  deriving Show

ex01 = Failure

ex02 = OK 3 2

safeDiv :: Double -> Double -> Int -> FailableDouble
safeDiv _ 0 q = Failure
safeDiv x y q = OK (x / y) q

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d q) = d

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a