{-# OPTIONS_GHC -Wall #-}
import Data.Char
--import Data.List
--module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = mod x 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
	| x < 1 = []
	| otherwise = (mod x 10):(toRevDigits (div x 10))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [h] = [2 * h]
doubleEveryOther (h1:h2:t) = (2 * h1):h2:(doubleEveryOther t)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (h:t) = val + (sumDigits t) where
	
	val
		|h > 9 = toInteger $ sum $ map digitToInt $ show h
		| otherwise = h


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = (==0) $ mod ((lastDigit x) + (sumDigits $ doubleEveryOther $ reverse $ properFormat $ x)) 10

properFormat :: Integer -> [Integer]
properFormat =  map toInteger . map digitToInt . show . dropLastDigit

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x peg1 peg2 peg3
	| x == 1 = [(peg3,peg2)]
	| otherwise = hanoi (x-1)
