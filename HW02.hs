{-# OPTIONS_GHC -Wall #-}
import Data.List
--module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches code1 code2 = length $ filter (\c -> (fst c) == (snd c)) (zip code1 code2)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors = map (+(-1)) . map length . group . sort . (++) [Red,Green,Blue,Yellow,Orange,Purple]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code1 code2 = sum $ map (\x -> min (fst x) (snd x)) (zip count1 count2) where
	count1 = countColors code1
	count2 = countColors code2


-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove code guess = Move guess (exact) (appr - exact) where
	exact = exactMatches guess code
	appr = matches guess code

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move guess _ _) code = move == (getMove code guess)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes 

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) allPegs
allCodes n = appendAll $ allCodes (n-1)

allPegs :: Code
allPegs = [Red,Green,Blue,Yellow,Orange,Purple]


appendAll :: [Code] -> [Code]
appendAll [] = []
appendAll (h:t) = map (:h) allPegs ++ (appendAll t)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve x = solveHelp x (getMove x (head allCode)) (tail allCode) where
	allCode = allCodes 6

solveHelp :: Code -> Move -> [Code] -> [Move]
solveHelp code x@(Move guess _ _)  allCode
	| code == guess = [getMove code guess]
	| otherwise = (getMove code guess):(solveHelp code (getMove code (head redux)) (tail redux))
	where redux = filterCodes x allCode

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
