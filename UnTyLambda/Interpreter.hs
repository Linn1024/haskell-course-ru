{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

alpha :: Term -> [Variable] -> Term
alpha (Var v) vr = (Var v)
alpha (App k l) vr = App (alpha k vr) (alpha l vr)
alpha (Lam v k) vr = (Lam new (alpha (subst k v (Var new))(vr ++ [new])))where new = (newname vr v) 
 
beta :: Term -> Term
beta (App (Lam v k) l) = subst reg v l where
	(Lam _ reg) = (alpha (Lam v k) (free (App (Lam v k) l)))
beta a = a

--- ...

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого 
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

hasReduct :: Term -> Bool
hasReduct (Var v) = False
hasReduct (Lam v k) = hasReduct k
hasReduct (App (Lam v k) l) = True
hasReduct (App k l) = (hasReduct k) || (hasReduct l)

hasReduct1 :: Term -> Bool
hasReduct1 (Var v) = False
hasReduct1 (Lam v k) = False
hasReduct1 (App (Lam v k) l) = True
hasReduct1 (App k l) = (hasReduct k) || (hasReduct l)


wh, no, wa, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
makeReductSa :: Term -> Term
makeReductSa (Var v) = (Var v)
makeReductSa (Lam v b) = (Lam v (makeReductSa b))
makeReductSa (App k l) = if (hasReduct l) then (App k (makeReductSa l)) else p
	where p = if (hasReduct k) then (App (makeReductSa k) l) else (beta (App k l))


sa 0 t = if (hasReduct t) then (error $ "Too long sequence at [" ++ show t ++ "]") else t
sa n t = sa (n - 1) (makeReductSa t)


-- Нормализация нормальным порядком
makeReductNo :: Term -> Term
makeReductNo (Var v) = (Var v)
makeReductNo (Lam v b) = (Lam v (makeReductNo b))
makeReductNo (App (Lam v k) l) = (beta (App (Lam v k) l))
makeReductNo (App k l) = if (hasReduct k) then (App (makeReductNo k) l) else p
	where p = (App k (makeReductNo l))



no 0 t = if (hasReduct t) then (error $ "Too long sequence at [" ++ show t ++ "]") else t
no n t = sa (n - 1) (makeReductNo t)

-- Редукция в слабую головную нормальную форму
makeReductWh :: Term -> Term
makeReductWh (App (Lam v k) l) = (beta (App (Lam v k) l))
makeReductWh (App k l) = if (hasReduct1 k) then (App (makeReductWh k) l) else p
	where p = (App k (makeReductWh l))
makeReductWh a = a


wh 0 t = if (hasReduct t) then (error $ "Too long sequence at [" ++ show t ++ "]") else t
wh n t = sa (n - 1) (makeReductWh t)



-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--	
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).
