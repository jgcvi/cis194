{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

z :: Num a => Poly a
z = z

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P a) == (P y) =  a == y
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P a) = help $ reverse a

help :: (Num a, Eq a, Show a) => [a] -> String
help x
	| x == [] = ""
	| head x == 0 = recurse
	| length x == 1 = (show $ head x) ++ recurse
	| length x == 2 = (show $ head x) ++ "x + " ++ recurse
	| otherwise = (show $ head x) ++ "x^" ++ (show $ (length x - 1)) ++ " + " ++ recurse
	where recurse = help $ tail x

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (plus' a b)

plus' :: (Num a) => [a] -> [a] -> [a]
plus' [] [] = []
plus' a [] = (head a):(plus' (tail a) [])
plus' [] a = (head a):(plus' (tail a) [])
plus' a b = (+) (head a) (head b) : (plus' (tail a) (tail b))
	
-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = foldr plus (P [0]) $ fmap P $ summate 0 (map (\x -> fmap (*x) b) a)

summate :: (Num a) => Int -> [[a]] -> [[a]]
summate _ [] = [[]]
summate n a = (take n (repeat 0) ++ head a) : (summate (n+1) (tail a))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P $ map negate a
    fromInteger x =  P $ [Prelude.fromInteger x]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined


-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P a) x = foldr (+) 0 $ map (\y -> fst y * snd y) $ zip a [x ^ n | n<-[0..(length a)]]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n a
	| n == 1 = deriv a
    	| otherwise = nderiv (n-1) (deriv a)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
	deriv (P a) =  P $ deriv' (tail a) 1

deriv' :: Num a => [a] -> a -> [a]
deriv' [] _ = []
deriv' (h:t) n = (n*h):(deriv' t (n+1))
