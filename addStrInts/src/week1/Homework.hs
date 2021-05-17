toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = (n `mod` 10):toDigitsRev (n `div` 10)
  | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:(y:zs))
  | even . length $ zs = x * 2:y:doubleEveryOther zs
  | otherwise = x:y * 2:doubleEveryOther zs

calDigits :: Integer -> Integer
calDigits n
  | n == 0 = 0
  | n > 0 = n `mod` 10 + calDigits (n `div` 10)
  | otherwise = error "param error, not positive number"

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:zs) = calDigits x + sumDigits zs

validate :: Integer -> Bool
validate n
  | (mod . sumDigits . doubleEveryOther . toDigits $ n) 10 == 0 = True
  | otherwise = False

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src goal temp = [(src, goal)]
hanoi n src goal temp = hanoi (n - 1) src temp goal
  ++ hanoi 1 src goal temp
  ++ hanoi (n - 1) temp goal src

-- hanoi tower with 4 
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 src goal _ _ = [(src, goal)]
hanoi4 2 src goal _ temp2 = [(src, temp2), (src, goal), (temp2, goal)]
hanoi4 n src goal temp1 temp2 = hanoi4 (n - 2) src temp1 goal temp2
  ++ [(src, temp2), (src, goal), (temp2, goal)]
  ++ hanoi4 (n - 2) temp1 goal src temp2
