module Sudoku where

import Data.Char

rows = "ABCD"
cols = "1234"
board = cross rows cols

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs


cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x,y]| x <- xs, y <- ys]

replacePointsWithZeros :: String -> String
replacePointsWithZeros [] = []
replacePointsWithZeros (x:xs)
    | x == '.' = '0' : replacePointsWithZeros xs
    | otherwise = x : replacePointsWithZeros xs

parseBoard :: String -> [(String, Int)]
parseBoard xs = zip board $ map digitToInt $ replacePointsWithZeros xs

unitList :: [[String]]
unitList = [cross rows [x] | x <- cols] ++ [cross [y] cols | y <- rows] ++ [cross (take 2 rows) (take 2 cols)] ++ [cross (take 2 rows) (drop 2 cols)] ++ [cross (drop 2 rows) (take 2 cols)] ++ [cross (drop 2 rows) (drop 2 cols)]

filterUnitList x = flip filter unitList $ containsElem x

units = [(square, filterUnitList square) | square <- board]

foldList :: [[a]] -> [a]
foldList = concat

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) 
    | containsElem x xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

peers :: [(String, [String])]
peers = zip board $ map (removeDuplicates . foldList . snd) units 