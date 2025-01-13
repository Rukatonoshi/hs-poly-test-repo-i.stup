module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = reversed (reverse lst)
	where
		reversed [] = REmpty
		reversed (x : xs) = (reversed xs) :< x

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show lst = "[" ++ nums ++ "]"
        where myShow REmpty = ""
              myShow (REmpty :< h) = show h 
              myShow (t :< h) = (myShow t) ++ "," ++ (show h)
              nums = myShow lst

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) (h :< t) REmpty = False
    (==) REmpty (h :< t) = False
    (==) (lh :< lt) (rh :< rt) = lt == rt && lh == rh  

instance Semigroup (ReverseList a) where
    (<>) lhs REmpty = lhs
    (<>) lhs (REmpty :< t) = lhs :< t
    (<>) lhs (h :< t) = (lhs <> h) :< t 

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap f (REmpty) = REmpty
    fmap f (h :< t) = (fmap f h) :< (f t)

instance Applicative ReverseList where
    pure e = REmpty :< e
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty
    (<*>) (h :< t) lst = (h <*> lst) <> (fmap t lst)   

instance Monad ReverseList where
    (>>=) REmpty f = REmpty
    (>>=) (h :< t) f = (h >>= f) <> (f t) 
