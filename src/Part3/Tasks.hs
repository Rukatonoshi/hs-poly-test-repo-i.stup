module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq lst =
    let digitCounts = countDigits lst
        (maxCount, _) = maxByCount digitCounts
    in maxCount
    where
        countDigits [] = []
        countDigits (x:xs) = mergeCounts (countDigitsInNumber x) (countDigits xs)
        countDigitsInNumber n = countDigitsInList (digits n)
        countDigitsInList [] = []
        countDigitsInList (d:ds) = insertCount d (countDigitsInList ds)
        insertCount d [] = [(d, 1)]
        insertCount d ((d', c):rest)
            | d == d' = (d', c + 1) : rest
            | otherwise = (d, 1) : (d', c) : rest
        mergeCounts [] ys = ys
        mergeCounts xs [] = xs
        mergeCounts ((d, c):xs) ((d', c'):ys)
            | d == d' = (d, c + c') : mergeCounts xs ys
            | d < d' = (d, c) : mergeCounts xs ((d', c'):ys)
            | otherwise = (d', c') : mergeCounts ((d, c):xs) ys
        maxByCount [] = (0, 0)
        maxByCount [(d, c)] = (d, c)
        maxByCount ((d, c):(d', c'):rest)
            | c > c' = maxByCount ((d, c):rest)
            | otherwise = maxByCount ((d', c'):rest)
        digits n = map (read . (:[])) (show n)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = traverse []
    where
        traverse acc [] = acc
        traverse acc (x:xs)
            | x `elem` acc = traverse acc xs
            | otherwise = traverse (x : acc) xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = grokAem [] l
	where
		grokAem acc [] = acc
		grokAem acc (x:xs) = grokAem (addToGroup (f x) x acc) xs
		addToGroup k v [] = [(k, [v])]
		addToGroup k v ((k', vs) : rest)
			| k == k' = (k, v : vs) : rest
			| otherwise = (k', vs) : addToGroup k v rest
