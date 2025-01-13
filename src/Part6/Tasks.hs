{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Data.Map hiding (drop, foldl, map, splitAt, take)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
    sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
} deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    initEye :: Int -> mx
    initZero :: Int -> Int -> mx
    (|*|) :: mx -> mx -> mx
    minor :: Int -> Int -> mx -> mx
    width :: mx -> Int
    height :: mx -> Int
    getRow :: Int -> mx -> [Int]
    getCol :: Int -> mx -> [Int]
    getAt :: Int -> Int -> mx -> Int

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    initEye _ = 1
    initZero _ _ = 0
    (|*|) = (*)
    minor _ _ _ = error "Incorrect matrix size"
    width _ = 1
    height _ = 1
    getRow _ mx = [mx]
    getCol _ mx = [mx]
    getAt _ _ mx = mx

instance Matrix [[Int]] where
    initEye h
        | h < 0 = error "Incorrect matrix size"
        | otherwise = map (row 0) [0..h-1]
        where
            row i x
                | i >= h = []
                | i == x = 1 : row (i + 1) x
                | otherwise = 0 : row (i + 1) x

    initZero w h
        | w < 0 || h < 0 = error "Incorrect matrix size"
        | otherwise = replicate h . replicate w $ 0

    mx1 |*| mx2
        | width mx1 /= height mx2 = error "Incorrect matrices sizes"
        | otherwise = map evalRow mx1
        where
            evalRow row = map (sum . zipWith (*) row . flip getCol mx2) [0..width mx2 - 1]

    minor row col mx
        | width mx < 2 || height mx < 2 = error "Incorrect matrix size"
        | otherwise = map (remove col) $ remove row mx
        where
            remove n xs = take n xs ++ drop (n + 1) xs

    width [] = 0
    width (h : _) = length h

    height mx = length mx

    getRow n mx = mx !! n

    getCol n mx = map colElem mx
        where
            colElem row = row !! n

    getAt row col mx = mx !! row !! col

instance Matrix (SparseMatrix Int) where
    initEye h
        | h < 0 = error "Incorrect matrix size"
        | otherwise = SparseMatrix h h (fromList . map eyeElem $ [0..h-1])
        where
            eyeElem x = ((x, x), 1)

    initZero w h
        | w < 0 || h < 0 = error "Incorrect matrix size"
        | otherwise = SparseMatrix w h mempty

    mx1 |*| mx2
        | width mx1 /= height mx2 = error "Incorrect matrices sizes"
        | otherwise = SparseMatrix mx2Width mx1Height result
        where
            result = foldl evalRow mempty [0 .. mx1Height - 1]
            mx1Height = height mx1
            evalRow elements row = foldl (`evalElem` row) elements [0 .. mx2Width - 1]
            mx2Width = width mx2
            evalElem elements row col
                | value == 0 = elements
                | otherwise = insert (row, col) value elements
                where
                    value = sum . zipWith (*) (getRow row mx1) $ getCol col mx2

    width (SparseMatrix w _ _) = w

    height (SparseMatrix _ h _) = h

    getRow n (SparseMatrix w _ elements) = map rowElem [0 .. w - 1]
        where
            rowElem col = findWithDefault 0 (n, col) elements

    getCol n (SparseMatrix _ h elements) = map colElem [0 .. h - 1]
        where
            colElem row = findWithDefault 0 (row, n) elements

    getAt row col (SparseMatrix _ _ elements) = findWithDefault 0 (row, col) elements

    minor row col (SparseMatrix w h elements)
        | w < 2 || h < 2 = error "Incorrect matrix size"
        | otherwise = SparseMatrix (w - 1) (h - 1) result
        where
            result = mapKeys correctIndex . filterWithKey predicate $ elements
            predicate (row', col') _ = row' /= row && col' /= col
            correctIndex (row', col')
                | row' < row && col' < col = (row', col')
                | row' < row = (row', col' - 1)
                | col' < col = (row' - 1, col')
                | otherwise = (row' - 1, col' - 1)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = initEye

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = initZero

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = (|*|)

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant mx
    | width mx == 0 = error "Incorrect matrix size"
    | width mx == 1 = getAt 0 0 mx
    | width mx == 2 = getAt 0 0 mx * getAt 1 1 mx - getAt 0 1 mx * getAt 1 0 mx
    | otherwise = sum [(-1) ^ i * getAt 0 i mx * determinant (minor 0 i mx) | i <- [0 .. height mx - 1]]
