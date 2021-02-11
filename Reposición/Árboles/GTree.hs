{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Práctica de Reposición 1
- Integrantes:
- Azpeitia García Karyme Ivette
- Dorantes Perez Brando
- Valencia Cruz Jonathan Josué
-}

module GTree where

import Data.List

data GTree a = Node a [GTree a] deriving (Eq,Show)
data BTree a = Void | Nodee a (BTree a) (BTree a) deriving(Eq,Show)

-- | size. Regresa el número de elementos de un árbol general.
size :: GTree a -> Int
size  (Node _ []) = 1
size (Node _ xs)  = 1 + sum(map size xs)

-- | depth. Regresa la profundidad de un árbol.
depth :: GTree a -> Int
depth (Node _ []) = 1
depth (Node _ xs) = 1 + maximum(map depth xs)

-- | tran. Transforma un árbol general a uno binario.
tran :: GTree a -> BTree a
tran (Node a [])      = (Nodee a Void Void)
tran (Node a [a'])    = (Nodee  a (tran a') Void)
tran (Node a xs)      = concatTree (tran (head xs))(tran (Node a (tail xs)))

-- | concatTree. Función que concatena dos árboles.
concatTree :: BTree a -> BTree a -> BTree a
concatTree Void Void         = Void
concatTree Void t            = t
concatTree t Void            = t 
concatTree (Nodee a t1 t2) t = Nodee a (concatTree t1 t) t2 

-- | mapg. map para un árbol general.
mapg :: (a->b)->GTree a -> GTree b
mapg f (Node a []) = Node (f a) []
mapg f (Node a xs) = Node (f a)[mapg f x | x<- xs]

-- | searchg. Verifica si un elemento está en un árbol general.
searchg :: (Eq a) => a -> GTree a -> Bool
searchg a (Node a' [])
   |a==a'                 = True
   |otherwise             = False
searchg a (Node a' [a'']) = searchg a (Node a' []) || searchg a a''
searchg a (Node a' xs)    = searchg a (Node a' []) || foldr (||) True (map (searchg a) xs)

-- | Árbol Prueba
test :: GTree Integer
test = Node 0 [Node 1 [Node 12 []], Node 2 [Node 22 [], Node 23 [Node 231 [], Node 232 []]], Node 3 []]

-- | Pruebas

size1 = size test
-- Regresa  9

depth1 = depth test
-- Regresa 4

tran1 = tran test
-- Regresa Nodee 1 (Nodee 12 (Nodee 22 (Nodee 2 (Nodee 231 (Nodee 23 (Nodee 232 (Nodee 0 (Nodee 3 Void Void) Void) Void) Void) Void) Void) Void) Void) Void

mapg1 = mapg (+1) test
-- Regresa Node 1 [Node 2 [Node 13 []],Node 3 [Node 23 [],Node 24 [Node 232 [],Node 233 []]],Node 4 []]

searchg1 = searchg 1 test
-- Regresa True
