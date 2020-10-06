{-# OPTIONS_GHC -Wall #-}
module Vakulenko05 where

type Graph = [[Int]]

--- Help Functions -------------------------------------

---------------- conver [Int] -> Int ------------------

fromArrToInt :: [Int] -> Int
fromArrToInt xs = foldl (\acc x -> (acc * 10) + x) 0 xs

---------------- conver Int -> [Int] ------------------

fromIntToArr :: Int -> [Int]
fromIntToArr 0 = []
fromIntToArr x = fromIntToArr (x `div` 10) ++ [x `mod` 10]

-----------------------------------------------------------


-- Task 1 ------------------------------------------

-- find min/max and set of possible n-digit Int

minNum :: Int -> Int
minNum num = fromArrToInt ([1] ++ take ((num*2)-1) (repeat 0))

maxNum :: Int -> Int
maxNum num = fromArrToInt( take ((num*2)) (repeat 9))

setOfN :: Int -> [[Int]]
setOfN num = map (fromIntToArr) [minNum num .. maxNum num]

intArrIsLucky :: [Int] -> Int -> Bool
intArrIsLucky xs num = if (sum xs) - sum (take (num) xs) == sum (take (num) xs)
 then True
 else False 

checkIntArrOnLucky :: Int -> [[Int]]
checkIntArrOnLucky num = [xs | xs <- (setOfN num), intArrIsLucky xs num == True]

makeIntFromArrInt :: Int -> [Int]
makeIntFromArrInt num = map (fromArrToInt) (checkIntArrOnLucky num)

toString :: Int -> String
toString 0 = []
toString x = (toString (div x 10)) ++ show(mod x 10)

lucky :: Int -> [String]
lucky num = if num > 0
 then map (toString) (makeIntFromArrInt num)
 else error "incorect n"


-- Task 2 ------------------------------------------

setOfT2 :: Int -> [[Int]]
setOfT2 num = map (fromIntToArr) [(fromArrToInt(take num (repeat 1)))..(fromArrToInt(take num (repeat num)))] 

isSet :: [Int] -> Bool
isSet [] = True
isSet (s:xs) | elem s xs = False
 | otherwise = isSet xs

haveZeroAnd :: [Int] -> Int -> Bool
haveZeroAnd xs num = (notElem 0 xs) && (all (<=num) xs)

setOfT2WithoutZeroAnd :: Int -> [[Int]]
setOfT2WithoutZeroAnd num = [xs | xs <- (setOfT2 num), haveZeroAnd xs num == True] 

checkOnRep :: Int -> [[Int]]
checkOnRep num = [xs | xs <- (setOfT2WithoutZeroAnd num), isSet xs == True]


isCorrect :: [Int] -> Bool
isCorrect xs = foldl1 (&&) [ abs(x-y)/=abs(xs!!x - xs!!y) | x <- [0..((length xs) - 2)], y <- [(x+1)..((length xs) - 1)]]

queens :: Int -> [[Int]]
queens num = [ xs| xs <- checkOnRep num, isCorrect xs == True]

-- Task 3 ---------------------------------------------


 
-- cond :: ([Int], [[Int]]) -> Bool
-- cond ([], _) = True
-- cond (_:[], _) = True
-- cond (_ , _) = False

-- step :: ([Int], [[Int]]) -> ([Int], [[Int]])
-- step ([], _) = error "empty list"
-- step (_:[], _ ) = error "empty list"
-- step (_:xs, yss) = (xs, xs:yss)

-- takeAllTails :: [Int] -> [[Int]]
-- takeAllTails xs = snd(until cond step (xs,[])) 

-- addHead :: Int -> [Int] -> [Int]
-- addHead myHead xs = myHead: xs

-- addHeadToArr :: [Int] -> [[Int]]
-- addHeadToArr xs = map (addHead (head xs)) (takeAllTails xs)

-- allPossibleCombination :: [Int] -> [[Int]]
-- allPossibleCombination xs = concat ((addHeadToArr xs): map addHeadToArr (takeAllTails xs))

-- test :: [Int] -> [[Int]]
-- test xs = [ xxs | xxs <- allPossibleCombination xs, ascendingOrderCheck xxs]

-- test2 :: Int -> [Int] -> [[Int]]
-- test2 num xs = [ xxs | xxs <- test xs num, ascendingOrderCheck xxs]

-- allPossibleCombinationWithTest2 :: [Int] -> Int -> [[Int]]
-- allPossibleCombinationWithTest2 xs num = concat (map (test2 num) (allPossibleCombination xs))

-- removeDuplicates :: [Int] -> [Int]
-- removeDuplicates = foldl (\seen x -> if x `elem` seen
-- then seen
-- else seen ++ [x]) []


sublists :: [Int] -> [[Int]]
sublists [] = [[]]
sublists (x:xs) = let yss = sublists xs
 in yss ++ map (x:) yss

ascendingOrderCheck :: [Int] -> Bool
ascendingOrderCheck xs 
 | null xs = False
 | null (tail xs) = True
 | ((head xs < head (tail xs)) && (length (tail xs)) > 0 ) = ascendingOrderCheck (tail xs)
 | otherwise = False


sublistsAsOrder :: [Int] -> [[Int]]
sublistsAsOrder xs = [xxs | xxs <- sublists xs, ascendingOrderCheck xxs && not (null xxs)]


lengthInArr :: [Int] -> Int
lengthInArr xs = maximum (map (length) (sublistsAsOrder xs))

allMaxSeq :: [Int] -> [[Int]]
allMaxSeq xs = [xxs | xxs <- sublistsAsOrder xs, length xxs == lengthInArr xs]


maxSeq :: [Int] -> [Int]
maxSeq xs = head (allMaxSeq xs)

maxLen :: [Int] -> Int
maxLen xs = length (maxSeq xs)


















