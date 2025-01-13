module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
-- Plus
(|+|) :: Term -> Term -> Term
(|+|) lhv rhv = BinaryTerm Plus lhv rhv
infixl 6 |+|
-- Minus
(|-|) :: Term -> Term -> Term
(|-|) lhv rhv = BinaryTerm Minus lhv rhv
infixl 6 |-|
-- Mult
(|*|) :: Term -> Term -> Term
(|*|) lhv rhv = BinaryTerm Times lhv rhv
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant v) = IntConstant v
replaceVar varName replacement (Variable v)
	| v == varName = replacement
	| otherwise = Variable v
replaceVar varName replacement (BinaryTerm op lhv rhv) =
	BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant v) = IntConstant v
evaluate (Variable v) = Variable v
evaluate (BinaryTerm op lhv rhv) =
    case (evaluate lhv, evaluate rhv) of
        (IntConstant l, IntConstant r) ->
            case op of
                Plus -> IntConstant (l + r)
                Minus -> IntConstant (l - r)
                Times -> IntConstant (l * r)
        _ -> BinaryTerm op (evaluate lhv) (evaluate rhv)
