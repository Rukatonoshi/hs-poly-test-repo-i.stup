module Part5.Tasks where

import Util (notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

-- Реализация myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x acc -> f x : acc) []

-- Реализация myConcatMap
myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (\x acc -> f x ++ acc) []

-- Реализация myConcat
myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

-- Реализация myReverse
myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

-- Реализация myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\x acc -> if p x then x : acc else acc) []

-- Реализация myPartition
myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\x (ts, fs) -> if p x then (x : ts, fs) else (ts, x : fs)) ([], [])
