{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero  = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. _ = Zero
n -. Zero = n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

natDiv :: Nat -> Nat -> Nat
natDiv Zero Zero = undefined
natDiv n Zero = undefined
natDiv Zero n = Zero
natDiv n m = (if' (natLt n m) Zero (natDiv(n -. m) m))


natMod :: Nat -> Nat -> Nat
natMod Zero Zero = undefined
natMod n Zero = undefined
natMod Zero n = Zero
natMod n m = (if' (natLt n m) m (natDiv(n -. m) m))




-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = Pair (natDiv n m) (natMod n m)


-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd Zero n = n
gcd n m = gcd(natMod m n) m

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat| Neg Nat deriving (Show,Read)

intZero   = Pos natZero  -- 0
intOne    = Pos natOne     -- 1
intNegOne = Neg natZero -- -1


intAbs :: Int -> Nat
intAbs (Pos a) = a
intAbs (Neg a) = a+.natOne

intSign :: Int -> Nat
intSign (Pos a) = natZero
intSign (Neg a) = natOne


-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = (Pos Zero)
intNeg (Pos (Succ a)) = (Neg a)
intNeg (Neg a) = (Pos (Succ a))

natToInt :: Nat -> Nat -> Int
natToInt Zero a = Pos a
natToInt a Zero = Pos Zero
natToInt natOne (Succ a) = Neg (a)


-- Дальше также как для натуральных
triToTri :: Tri -> Tri -> Bool -> Tri
triToTri EQ EQ True = EQ
triToTri LT EQ False = GT
triToTri GT EQ False = LT
triToTri LT EQ True = LT
triToTri GT EQ True = GT
triToTri a GT _ = LT
triToTri a LT _ = GT


intCmp :: Int -> Int -> Tri
intCmp n m = triToTri (natCmp (intAbs n) (intAbs m)) (natCmp (intSign n) (intSign m)) (natEq (intSign n) Zero)


--intCmp = undefined

triToBool :: Tri -> Tri -> Bool
triToBool EQ EQ = True
triToBool LT LT = True
triToBool GT GT = True



intEq :: Int -> Int -> Bool
intEq n m= (if' (triToBool(intCmp n m) EQ) True False)

intLt :: Int -> Int -> Bool
intLt n m= (if' (triToBool(intCmp n m) LT) True False)


infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле

intAdd :: Int -> Int -> Bool -> Int
intAdd n m True = (natToInt (intSign n) ((intAbs n) +. (intAbs m))) 
intAdd n m False = (natToInt (intSign n) ((intAbs n) -. (intAbs m))) 


(.+.) :: Int -> Int -> Int
n .+. m = intAdd (if' (natLt (intAbs n) (intAbs m)) m n) (if' (natLt (intAbs n) (intAbs m)) n m) (if' (natEq (intSign n) (intSign m)) True False)

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
n .*. m = natToInt (if' (natEq (intSign n) (intSign m)) Zero natOne) ((intAbs n) *. (intAbs m))


-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat x y)= (Rat (natToInt (intSign x)(y)) ( intAbs x)) 

ratNumb :: Rat -> Int
ratNumb (Rat x y) = x

ratDec :: Rat -> Int
ratDec (Rat x y) = Pos y



-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp n m = (if' (ratEq n m) EQ $ (if' (ratLt n m)) LT GT)

ratEq :: Rat -> Rat -> Bool
ratEq (Rat x y) (Rat a b)= (intEq x a) && (natEq y b)

ratLt :: Rat -> Rat -> Bool
ratLt n m= intLt((ratNumb n) .*. (ratDec m))((ratNumb m) .*. (ratDec n))

norm  :: Rat -> Rat
norm (Rat n m) = (Rat (natToInt(intSign n)((natDiv (intAbs n)(gcd (intAbs n) (m))))) (natDiv (m) ((gcd (intAbs n) (m)))))

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
n %+ m = norm (Rat((((ratNumb n) .*. (ratDec m)) .+. ((ratNumb m) .*. (ratDec n)))) (intAbs((ratDec m) .*. (ratDec n))))

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
n %* m = norm (Rat((ratNumb n) .*. (ratNumb m)) (intAbs((ratDec m) .*. (ratDec n))))

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
