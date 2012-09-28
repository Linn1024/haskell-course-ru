module ITMO.Prelude.Tree where
data Tree a = Node a (Tree a) (Tree a) | Leaf

empty :: Tree a
empty = Leaf

insert :: a -> Tree a -> Tree a
insert a x = Node a x Leaf

addLeft :: a-> Tree a -> Tree a
addLeft a Leaf = Node a Leaf Leaf
addLeft a (Node x left right) = Node x (addLeft a left) right

addRight :: a-> Tree a -> Tree a
addRight a Leaf = Node a Leaf Leaf
addRight a (Node x left right) = Node x left (addRight a left)

-- Поворот влево
--    p              q 
--   / \            / \
--  l   q    ->    p   c 
--     / \        / \
--    b   c      l   b

rotateL :: Tree a -> Tree a
rotateL (Node p l (Node q b c)) = (Node q (Node p l b) c)

-- Поворот вправо
--      q          p
--     / \        / \
--    p   r  ->  a   q
--   / \            / \
--  a   b          b   r

rotateR :: Tree a -> Tree a
rotateR (Node q (Node p a b) r) = (Node p a (Node q b r))

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Leaf = Leaf
treeMap f (Node a left right) = (Node (f a))((treeMap f left))((treeMap f right))

treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr f z Leaf = z
treeFoldr f z (Node a left right) = treeFoldr f (f a (treeFoldr f z right)) left
