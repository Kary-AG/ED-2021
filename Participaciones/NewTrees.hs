module NewTrees where
import Data.List

data BTree a = Void
             | Node a (BTree a) (BTree a) deriving (Show , Eq)

data BT a = VoidL | Leaf a 
          | NodeL a (BT a) (BT a) deriving (Show , Eq)

l = [1,1,1,2,2,3] -- => [(1,3), (2,2), (3,1)]
-- a = (b, Int)

inserts :: (Num b, Ord a) => a -> BTree (a,b) -> BTree (a, b)
inserts e Void             = Node (e, 1) Void Void
inserts e (Node (a,b) l r) = if e == a
                            then Node (a, b + 1) l r
                            else if e < a
                                 then Node (a, b) (inserts e l) r
                                 else Node (a, b) l (inserts e r)


-- | add. Agrega un elemento a un árbol binario de manera ordenada.
add :: Ord a => a -> BTree a -> BTree a
add  n Void = (Node n Void Void)
add n (Node a x y)
  | n > a || n == a = (Node a x (add n y))
  | n < a = (Node a (add n x) y)

-- | fromList. Función que pasa una lista a un árbol dada una función para agregar.
fromList :: (t -> BTree a1 -> BTree a2) -> [t] -> BTree a2
fromList f [] = Void
fromList f [a] = f a Void
fromList f (x:xs) = concatTree (f x Void) (fromList f xs)

-- | concatTree. Función que concatena dos árboles.
concatTree :: BTree a -> BTree a -> BTree a
concatTree Void Void = Void
concatTree Void t = t
concatTree t Void = t
concatTree (Node a t1 t2) t = Node a (concatTree t1 t) t2 

-- | Rotación derecha
rotRight :: BTree a -> BTree a
rotRight (Node a (Node b t1 t2) t3) = Node b t1 (Node a t2 t3)  

-- | Rotación izquierda
rotLeft :: BTree a -> BTree a
rotLeft (Node a t1 (Node b t2 t3))= (Node b (Node a t1 t2) t3 
