{-# OPTIONS_GHC -Wall #-}
module Vakulenko02 where

-- Task 1 -----------------------------------------

sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs 
-- Task 2 -----------------------------------------

factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

-- Task 3 -----------------------------------------

concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys = foldr (:) ys xs

-- Task 4 -----------------------------------------

-- HELP function - вставляє новий елемент v у впорядкований список xs
insert :: [Integer] -> Integer -> [Integer]
insert xs v = if null xs
 then [v]
 else if head xs > v
 then (v:xs)
 else head xs: insert (tail xs) v
----------------------------------------------------

sortInsert :: [Integer] -> [Integer]
sortInsert xs = foldl insert [] xs
 

-- Task 5 -----------------------------------------

map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 k xs ys = if null xs
 then []
 else if null ys 
 then []
 else (k (head xs) (head ys)) : (map2 k (tail xs) (tail ys))

-- Task 6 -----------------------------------------

expPart :: Integer -> Integer -> Double
expPart m n = if n > 0 
 then ((fromIntegral) m^n / (fromIntegral) (factorial n)) + (expPart m (n - 1))
 else 0

-- Task 7 -----------------------------------------

triangle :: [Integer]
triangle = scanl1 (+) [n | n <- [1 ..]]

-- Task 8 -----------------------------------------

piramid :: [Integer]
piramid = scanl1 (+) [n * n | n <- [1 ..]]








