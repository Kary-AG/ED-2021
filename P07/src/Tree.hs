{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Practica 5: Nuestras estructuras. listas
- Integrantes:
- Azpeitia García Karyme Ivette
- Dorantes Perez Brando
- Valencia Cruz Jonathan Josué
-}
module BTree where

import Data.List
data BTree a = Void | Node a (BTree a) (BTree a) deriving(Show,Eq)

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | nNodes. Regresa el número de nodos de un árbol.
nNodes :: BTree a -> Int
nNodes             Void = 0
nNodes     (Node a x y) = 1 + nNodes(x) + nNodes(y)

-- | nLeaves. Regresa el número de hojas de un árbol.
nLeaves :: BTree a -> Int
nLeaves              Void = 0
nLeaves (Node a Void Void)= 1
nLeaves      (Node a x y) = nLeaves x + nLeaves y

-- | nni.Regresa el número de nodos internos de un árbol.
nni :: BTree a -> Int
nni             Void  = 0
nni (Node a Void Void)= 0
nni      (Node a x y) = 1 + nni x + nni y

-- | containsU. Nos dice si un elemento está contenido en un árbol que no necesariamente está ordenado.

containsU :: Int -> BTree Int -> Bool
containsU n Void = False
containsU n (Node a x y)
  | n == a = True
  | n /= a = containsU n x || containsU n y

-- | containsU. Nos dice si un elemento está contenido en un árbol ordenado.

contains :: Int -> BTree Int -> Bool
contains n Void = False
contains n (Node a x y) 
  | n == a = True
  | n > a  = contains n y
  | n < a  = contains n x

-- | inorder. Recorrido inorder.
inorder :: BTree a -> [a]
inorder               Void = []
inorder (Node a Void Void) = [a]
inorder       (Node a x y) =  inorder x ++ [a] ++ inorder y

-- | preorder. Recorrido preorder
preorder :: BTree a -> [a]
preorder               Void = []
preorder (Node a Void Void) = [a]
preorder       (Node a x y) =  a:(preorder x ++ preorder y)

-- | postorder. Recorrido postorder
posorder :: BTree a -> [a]
posorder Void = []
posorder (Node a Void Void) =[a]
posorder (Node a x y) =  inorder y ++ [a] ++ inorder x

-- | add. Agrega un elemento a un árbol binario de manera ordenada.
add :: Ord a => a -> BTree a -> BTree a
add  n Void = (Node n Void Void)
add n (Node a x y)
  | n > a || n == a = (Node a x (add n y))
  | n < a = (Node a (add n x) y)

-- | fromList. Pasa una lista a un árbol de forma ordenada.
fromList :: Ord a => [a] -> BTree a
fromList    [] = arbolitos[]
fromList   [a] = arbolitos[a]
fromList    xs = arbolitos(from (fro1(half1 xs)) (fro1(half2 xs)))

-- | fro1. Función Auxiliar funciona como fromList pero devuelve una lista.
fro1 [] = []
fro1 [a]= [a]
fro1 xs = from (fro1(half1 xs)) (fro1(half2 xs))

-- | from. Función Auxilia ordena los elementos de la lista.
from :: Ord a => [a] -> [a] ->[a]
from  xs [] = fro1 xs
from  [] ys = fro1 ys
from (x:xs)(y:ys)
  |  x <= y = (x:from xs (y:ys))
  | otherwise = (y:(from (x:xs) ys))
  
-- | arbolitos. Pasa una lista a un árbol.
arbolitos :: [a] -> BTree a
arbolitos    [] = Void
arbolitos   [a] = (Node a Void Void)
arbolitos (x:xs)= (Node x (arbolitos(half1 xs)) (arbolitos(half2 xs)))


-- | half1. Regresa la primera mitad de elementos en una lista.
half1:: [a] ->[a]
half1  [] = []
half1 [a] = [a]
half1  xs = take (div (length xs) 2) xs

-- | half2. Regresa la segunda  mitad de elementos en una lista.
half2:: [a] ->[a]
half2  [] = []
half2 [a] = [a]
half2  xs = drop (div (length xs) 2) xs

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------
t1=Node 33 ( Node 15 ( Node 10 ( Node 5 Void Void ) ( Node 12 Void Void ) ) ( Node 20 ( Node 18 Void Void ) ( Node 29 Void Void ) ) )( Node 47 ( Node 38 ( Node 36 Void Void ) ( Node 39 Void Void ) )( Node 51 ( Node 49 Void Void ) ( Node 100 Void Void ) ) )


nNodes1   = nNodes t1

nLeaves1  = nLeaves t1

nni1      = nni t1

containsU1 = containsU 64 t1

contains1 = contains 39 t1

inorder1 = inorder t1

preorder1 = preorder t1

posorder1 = posorder t1

add1 = add  3 t1

fromList1 = fromList (preorder t1) 
