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
fromList ::(Ord a)=> [a] -> BTree a                                                                                 
fromList [] = Void                                                                                                  
fromList [a] = Node a Void Void                                                                                     
fromList (x:xs) = arbolitos(from (x:xs))                                                                            

-- | Pasa una lista a un árbol.
arbolitos :: Ord a => [a] -> BTree a                                                                                
arbolitos [] = Void                                                                                                 
arbolitos [a] = Node a Void Void                                                                                    
arbolitos (x:xs) = Node x (Void)(arbolitos xs)                                                                      
                                                                                                                    
-- | from. Ordena una lista usando la función f                                                                     
from :: Ord a=> [a]->[a]                                                                                            
from [] = []                                                                                                        
from (x:xs) = f xs x [] 

-- | Ordena la lista                                                                                                
f:: Ord a => [a]->a->[a]->[a]                                                                                       
f [] x l = x : from l                                                                                               
f (x:xs) y ys                                                                                                       
  |x<y  = f xs x (y:ys)                                                                                             
  |otherwise = f xs y (x:ys)   


