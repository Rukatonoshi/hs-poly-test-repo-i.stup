module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x
    | abs x <= 2 * pi = taylorSin 0.0 32 x
    | otherwise = taylorSin 0.0 32 (norm x)
    where
        taylorSin acc 0 x = acc + x
        taylorSin acc n x = taylorSin (acc + (((fromInteger $ hPow 1 (-1) n) / (fromInteger $ hFact 1 (2 * n + 1))) * (hPow 1.0 x (2 * n + 1)))) (n - 1) x
        hPow acc _ 0 = acc
        hPow acc base exp = hPow (base * acc) base (exp - 1)
        hFact acc 0 = acc
        hFact acc n = hFact (n * acc) (n - 1)
        norm x = x - 2 * pi * (fromInteger $ floor (x / (2 * pi)))

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x
	| abs x <= 2 * pi = taylorCos 0.0 32 x
	| otherwise = taylorCos 0.0 32 $ norm x
	where
		taylorCos acc 0 _ = acc + 1.0
		taylorCos acc n x = taylorCos (acc + (((fromInteger $ hPow 1 (-1) n) / (fromInteger $ hFact 1 $ 2 * n)) * (hPow 1.0 x $ 2 * n))) (n - 1) x
		hPow acc _ 0 = acc
		hPow acc base exp = hPow (base * acc) base (exp - 1)
		hFact acc 0 = acc
		hFact acc n = hFact (n * acc) (n - 1)
		norm x = x - 2 * pi * (fromInteger $ floor $ (x / (2 * pi)))

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b $ (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- високосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y
	| m < 1 || m > 12 = False
	| d < 1 || d > (case m of
		2 -> if (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0 then 29 else 28
		4 -> 30
		6 -> 30
		9 -> 30
		11 -> 30
		_ -> 31) = False
	| otherwise = True

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow base exp
	| exp == 0 = 1
	| exp == 1 = base
	| otherwise = base * myPow base (exp - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
	| n <= 1 = False
	| n == 2 = True
	| even n = False
	| otherwise = not (any (\x -> n `mod` x == 0) [3, 5 .. floor (sqrt (fromInteger n))])

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = (/2) $ abs $ pSum 0.0 points $ head points
	where
		pSum n [(xn, yn)] (x0, y0) = n + xn * y0 - x0 * yn
		pSum n ((xi, yi) : (xj, yj) : p) p0 = pSum (n + xi * yj - xj * yi) ((xj, yj) : p) p0

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
	| a + b <= c || a + c <= b || b + c <= a = -1
	| a == b && b == c = 2
	| a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = 2
	| a^2 + b^2 < c^2 || a^2 + c^2 < b^2 || b^2 + c^2 < a^2 = 0
	| otherwise = 1
