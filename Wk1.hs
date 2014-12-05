module Wk1(validate, sumDigits, doubleEveryOther, doubleEvenElements, recurseList, doubleTail, toDigits) where

lastDigit :: Int -> Int
lastDigit n = n `mod` 10


dropLastDigit :: Int -> Int
dropLastDigit n = n `div` 10


toDigits :: Int -> [Int]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]


doubleTail :: [Int] -> [Int]
doubleTail x
  | (length x) == 2 = take 1 x ++ [(last x) * 2]
  | otherwise       = x


recurseList :: [Int] -> [Int]
recurseList x
  | (length x) >= 2  = doubleEvenElements (drop 2 x)
  | otherwise        = []


doubleEvenElements :: [Int] -> [Int]
doubleEvenElements x = doubleTail (take 2 x) ++ recurseList x


doubleEveryOther :: [Int] -> [Int]
doubleEveryOther x = reverse (doubleEvenElements (reverse x))


sumDigits :: [Int] -> Int
sumDigits x = sum (init x ++ toDigits (last x))


validate :: Int -> Bool
validate x
  | ((sumDigits $ doubleEveryOther $ toDigits x) `mod` 10) == 0 = True
  | otherwise = False

--validate x  = ((sumDigits (doubleEveryOther( toDigits(x) ))) `mod` 10) == 0


