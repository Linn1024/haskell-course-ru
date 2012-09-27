{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons (_)(a)) = (Succ (length a))

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ a = a
(Cons a ab) ++ x  = (Cons a $ ab ++ x)  

-- Список без первого элемента
tail :: List a -> List a
tail (Cons a ab)= ab 

-- Список без последнего элемента
init :: List a -> List a
init (Cons a Nil) = Nil
int (Cons a ab) = (Cons a $ init ab)

-- Первый элемент
head :: List a -> a
head (Cons a ab)= a

-- Последний элемент
last :: List a -> a
last (Cons a Nil) = a
last (Cons a ab) = last ab	

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero _ = Nil
take _ Nil = Nil
take (Succ n) (Cons a ab) = (Cons a) (take (n) ab)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero ab= ab
drop _ Nil= Nil
drop (Succ n)(Cons a ab) = drop n ab



-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a ab) = case p a of
	True -> Cons a (filter p ab)
	False ->(filter p ab)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a ab) = case p a of
	Just b-> Cons b (gfilter p ab)
	Nothing ->(gfilter p ab)

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a ab)= case p a of
	True -> (Cons a (takeWhile (p) (ab)))
	False -> Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a ab) = case p a of 
	False -> ab
	True -> (dropWhile (p) (ab))

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p (Nil) = Pair (Nil)(Nil)
span p (Cons a ab) = case p a of
	False -> Pair (Nil) (Cons a ab)
	True -> Pair (Cons (a) (fst(span p ab))) (snd(span p ab))

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p (Nil) = Pair (Nil)(Nil)
break p (Cons a ab) = case p a of
	True -> Pair (Nil) (Cons a ab)
	False -> Pair (Cons (a) (fst(span p ab))) (snd(span p ab))

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a ab) !! Zero = a
(Cons a ab) !! (Succ n) = ab !! n 

-- Список задом на перёд
reverse :: List a -> List a
reverse (Cons a Nil) = (Cons a Nil)
reverse (Cons a ab) = reverse(ab) ++ (Cons a Nil)  

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons a ab) = subsequences ab ++ (map (Cons a)(subsequences ab)) 

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil
permutations (Cons a ab) = perm Nil a ab where
    perm left m Nil = map (Cons m)  (permutations left)
    perm left m (Cons r rx) = (map (Cons m) $ permutations $ left ++ (Cons r rx)) ++ (perm (Cons m left) r rx) 

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = (Cons a (repeat a))

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons a ab) = foldl f (f z a) ab 

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons a ab) = (Cons (f z a) (scanl f (f z a) ab))	

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons a ab) = (f (a) (foldr f z ab)) 

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr f z Nil = z
scanr f z (Cons a ab) = Cons (f a (head (scanr f z ab))) (scanr f z ab)

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a ab) = Cons (f a) (map (f)(ab))

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons a ab) = (f a) ++ (concatMap f ab)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (Cons a ab)(Cons c cd) = Cons (Pair a c) (zip(ab)(cd))

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith p _ Nil = Nil
zipWith p Nil _ = Nil
zipWith p (Cons a ab)(Cons c cd) = Cons (p a c) (zipWith p (ab)(cd))
