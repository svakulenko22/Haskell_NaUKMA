{-# OPTIONS_GHC -Wall #-}
module Vakulenko01 where

-- Task 1 ---------------------------------	

factorial :: Integer -> Integer
factorial  0 = 1
factorial  n = n * factorial(n - 1)

-- Task 2 ----------------------------------

listSum :: [Int] -> [Int] -> [Int]

listSum xs ys = if (not(null xs)) && (not(null ys))
	            then (head xs) + (head ys) : (listSum (tail xs) (tail ys))
	            else if (not(null xs)) && (null ys)
	            then head xs + 0 : (listSum (tail xs) (ys))
	            else if (not(null ys)) && (null xs)
	            then head ys + 0 : (listSum (tail ys) (xs))
	            else []

-- listSum xs ys = if (length xs) == (length ys)
--                 then map (uncurry (+)) $ zip xs ys


-- Task 3 ----------------------------------


oddEven :: [Int] -> [Int] 
oddEven xs = if (null xs)
           then []
           else if null (tail xs) 
           then xs  
           else (head (tail xs) : (head xs : oddEven (tail (tail xs))))


-- Task 4 ----------------------------------

position ::  Int -> [Int] -> Int
position n xs = if (not (elemM n xs)) || null xs
	            then negate 1
	            else if (head xs) == n 
                then 0
                else 1 + (position n (tail xs))


 -- Task 5 ----------------------------------
 
set :: [Int] -> [Int] 
set xs = if not (elemM (head xs) (tail xs)) && lengthM (tail xs) == 0
         then head xs: tail xs
         else if not (elemM (head xs) (tail xs))
	     then head xs: set (tail xs)
         else set(tail xs)

-- Task 6 ----------------------------------

union :: [Int] -> [Int] -> [Int]
union xs ys = if null xs then ys
	          else set (head xs: addM (tail xs) ys) 

-- Task 7 ----------------------------------

intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = if null xs
	                 then set ys
	                 else set (head xs: interAdd(tail xs) ys)


-- Task 8 ----------------------------------

factorialsM :: [Integer]
factorialsM = [factorial(y) | y <- [1..]]

-- divisors x = [y | y <- [1..x], mod x y == 0] - CS01List slide - 17 
       
---- addFunctions -------------------

addM :: [Int] -> [Int] -> [Int]
addM xs ys = if null xs then ys
	         else head xs: addM (tail xs) ys     

-- CS01BeginWork slide - 17 

--------------------------------------------

lengthM           :: [a] -> Int
lengthM []        =  0
lengthM (_:l)     =  1 + lengthM l

-- https://www.haskell.org/onlinereport/standard-prelude.html

-- lengthM xs = if null xs 
--              then 0 
--              else 1 + lengthM (tail xs) 

-------------------------------------------------

interAdd :: [Int] -> [Int] -> [Int]
interAdd xs ys = if lengthM (tail xs) == 0 && elemM (head xs) ys
	             then head xs: tail xs
	             else if elemM (head xs) ys
	             then head xs: interAdd (tail xs) ys
	             else interAdd (tail xs) ys

-- intersection	of two list with duplicates of the first list    

-------------------------------------------------  

elemM :: Int -> [Int] -> Bool   
elemM x xs = if null xs 
	         then False
	         else if ((head xs) - x) == 0
	         then True
	         else elemM x (tail xs)


-------------------------------------------------