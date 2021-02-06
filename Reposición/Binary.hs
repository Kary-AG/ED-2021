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

module Binary where

import Compuertas

data Binary = Uno | Cero deriving (Eq)

instance Show Binary where
  show Uno  = "1"
  show Cero = "0"

instance Compuerta Binary where

  (.!.) Uno  = Uno 
  (.!.) Cero = Cero 
  
  (.¬.) Uno  = Cero
  (.¬.) Cero = Uno

  (.^.) Cero _  = Cero
  (.^.) _ Cero  = Cero
  (.^.) Uno Uno = Uno

  (.|.) _ Uno      = Uno
  (.|.) Uno _      = Uno
  (.|.) Cero Cero  = Cero

  -- xor
  p .+. q
    | p == q    = Uno
    | otherwise = Cero

  -- nand
  p .*. q
    | p == q = Cero
    | otherwise = Uno

  -- nor
  p .~. q
    | p /= q   = Cero
    | p == q = (.¬.) p

funcB :: (Binary -> Binary -> Binary) -> [Binary] -> [Binary] -> [Binary]
funcB f [] []        = []
funcB f [a][b]       = [f a b]
funcB f (x:xs)(y:ys) = [f x y]++ funcB f xs ys

funcU :: (Binary -> Binary) -> [Binary] -> [Binary]
funcU f []     = []
funcU f [a]    = [f a]
funcU f (x:xs) = [f x] ++ funcU  f xs


---------------------------------------------------------------------------------
--                                PRUEBAS                                      --
---------------------------------------------------------------------------------

l1 = [Uno, Cero, Uno, Cero]
l2 = [Cero, Uno, Cero, Uno]

pruebaU1 = funcU (.¬.) l1
-- regresa: l2

pruebaU2 = funcU (.!.) l1
-- regresa: l1

pruebaB1 = funcB (.^.) l1 l2
-- regresa: [0,0,0,0]

pruebaB2 = funcB (.|.) l1 l2
-- regresa: [1,1,1,1]
