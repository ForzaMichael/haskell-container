biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

sumtorial :: Int -> Int
sumtorial 0 = 0
sumtorial n = n + sumtorial (n -1)

isEven :: Integer -> Bool
isEven n
  | even n = True
  | otherwise = False

hailstone :: Integer -> Integer
hailstone n
  | isEven n = n `div` 2
  | otherwise = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

f :: Int -> Int -> Int -> Int
f x y z = x + y + z

ex :: Int -> Int
ex = f 1 2

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

genList :: Integer -> [Integer]
genList n = [1 .. n]

getListLength :: [Integer] -> Integer
getListLength [] = 0
getListLength (_ : xs) = 1 + getListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x : y : zs) = (x + y) : sumEveryTwo zs

hailstoneLen :: Integer -> Integer
hailstoneLen n = getListLength (hailstoneSeq n) -1