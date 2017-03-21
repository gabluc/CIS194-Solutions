module Homework1.Solution where

--Credit card

--Exercise 1

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x < 0     = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]


toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x


toDigitsRev' :: Integer -> [Integer]
toDigitsRev' 0 = []
toDigitsRev' x
  | x < 0 = []
  | otherwise = x `mod` 10 : toDigitsRev' (x `div` 10)



--Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = let double _ [] = []
                          double True (x:xs) = (x*2) : double False xs
                          double False (x:xs) = x : double True xs
                      in reverse $ double False (reverse xs)



--Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | check == 2 = div x 10 + mod x 10 + sumDigits xs
  | otherwise  = x + sumDigits xs
  where check = floor $ log (fromIntegral x)



validate :: Integer -> Bool
validate = (check . sumDigits . doubleEveryOther . toDigits)
  where check x = (mod x 10) == 0




--Hanoi tower

type Peg = String
type Move = (Peg,Peg)

--Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
                  
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ t _ = []
hanoi height a b c = hanoi (height-1) a c b ++
                     moveDisk a b ++
                     hanoi (height-1) c b a
                     where moveDisk from to = [(from,to)]


--frameStewart :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
--frameStewart n r = let k = n - round(sqrt(2*n+1)) + 1

