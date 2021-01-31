{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Ejercicio Semanal 3: Funciones de orden superior y recursividad
- Integrantes:
- Azpeitia Garcia Karyme Ivette
-
-}

module RecFu where

import Data.Char
import Data.List

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Función recursiva que recibe un número y devuelve True si es par
-- o False e.o.c. Nota: Debes usar a 'impar' para implementarla.
par :: Int -> Bool
par 0 = True
par n = impar (n-1)

-- | Función recursiva que recibe un número y devuelve True si es
-- impar o False e.o.c. Nota: Debes usar a 'par' para implementarla.
impar :: Int -> Bool
impar 0 = False
impar n = par (n-1)

-- | Toma los primeros 5 elementos de una lista y devuelve la reversa
-- de esa lista de 5 elementos. Si la lista tiene menos de 5 elementos
-- devuelve una lista vacía
reversa5 :: [Int] -> [Int]
reversa5 xs
  | length xs < 5 = []
  | otherwise = reversa(take 5 xs) 

-- | Función auxiliar recibe una lista y devuelve la reversa

reversa :: [Int] -> [Int]
reversa     [] = []
reversa (x:xs) = reversa xs ++ [x]

-- | Recibe un entero y devuelve la suma de sus dígitos
sumaDigitos :: Int -> Int
sumaDigitos x = sumaDigs (digs x)

-- | Función auxiliar, recibe un entero y  devuelve una lista con cada digito del número dado
digs :: Integral x => x -> [x]
digs  0 = []
digs x
  | x>0 = digs (x `div` 10) ++ [x `mod` 10]
  | x<0 = digs ((x+(-2*x)) `div` 10) ++ [(x+(-2*x)) `mod` 10]

-- | Función auxiliar recibe una lista de enteros y devuelve su suma
sumaDigs :: [Int] -> Int
sumaDigs     [] = 0
sumaDigs    [x] = x
sumaDigs (x:xs) = x + sumaDigs xs

-- | Recibe una lista de enteros y multiplica por dos el elemento que
-- se encuentre cada dos posiciones empezando por el final.
duplicaCadaDos :: [Int] -> [Int]
duplicaCadaDos xs = reversa (map2 (2*) (reversa xs)) 

-- | Función auxiliar,recibe una función y una lista, regresa la lista módificada cad dos digitos con la función.

map2 :: (a -> a) -> [a] -> [a]
map2       _ [] = []
map2      _ [x] = [x]
map2 f (x:y:xs) = [x] ++  f y : map2 f xs 

-- | Función recursiva que recibe dos enteros n y m y devuelve la
-- potencia de n^m
eleva :: Int -> Int -> Int
eleva n 0 = 1
eleva n 1 = n
eleva n m = n * ( eleva n (m-1))

-- | Función recursiva que recibe dos enteros n y m y devuelve la
-- suma de n+m. 
suma :: Int -> Int -> Int
suma n 0 = n
suma n 1 = succ(n)
suma n (-1) = pred(n)
suma n m
  | n> 0 && m > 0 = succ (suma n (pred(m)))
  | n< 0 && m < 0 = succ (suma n (succ(m)))

-- | Función recursiva que recibe dos enteros n y m y devuelve la
-- resta de n-m. NOTA Solo sirve en positivos.
resta :: Int -> Int -> Int
resta a b
  | a == b = 0
  | a > b  = succ(resta a (succ b ))
  | b > a  = succ(resta b (succ a ))

-- | Función recursiva que recibe dos enteros n y m y devuelve la
-- multiplicacion de n*m
multiplica :: Int -> Int -> Int
multiplica n 0 = 0
multiplica n 1 = n
multiplica n m = suma n  (multiplica n (m-1))

-- | Función recursiva que recibe un entero y calcula su factorial.
fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact n = n * fact(n-1)

-- | Función recursiva que calcula números de fibonacci.
fibonacci :: (Eq a, Num a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- | Función recursiva que es la función de Ackerman.
ackerman :: Int -> Int -> Int
ackerman        0 n = succ n
ackerman m n
  | n == 0 && m > 0 = ackerman (resta m 1) 1
  | m > 0  && n > 0 = ackerman (resta m 1) (ackerman m (resta n 1))

-- | myFilter. Función que emula a filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter x (y:ys)
  | x y       = y : myFilter x ys
  | otherwise = myFilter x ys 

-- | myMap. Función que emula a map.
myMap :: (a -> b) -> [a] -> [b]
myMap    _ [] = []
myMap f (x:xs)= f x : myMap f xs
--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
--------------------------------------------------------------------------------

par1 = par 2
-- Regresa: True

par2 = par 200
-- Regresa: True

par3 = par 33
-- Regresa: False

impar1 = impar 2
-- Regresa: False

impar2 = impar 200
-- Regresa: False

impar3 = impar 33
-- Regresa: True

reversa51 = reversa5 [1,2,3]
-- Regresa: []

reversa52 = reversa5 [n | n <- [1..100], (mod n 2) == 0]
-- Regresa: [10,8,6,4,2]

sumaDigitos1 = sumaDigitos 15623
-- Regresa: 17

sumaDigitos2 = sumaDigitos (-20)
-- Regresa: 2

duplicaCadaDos1 = duplicaCadaDos [1,2,3,4]
-- Regresa: [2,2,6,4]

duplicaCadaDos2 = duplicaCadaDos [3,7,5,8,2]
-- Regresa: [3,14,5,16,2]

duplicaCadaDos3 = duplicaCadaDos []
-- Regresa: []

eleva1 = eleva 8 6
--Debe regresar 262144

eleva2 = eleva 2 10
--Debe regresar 1024

suma1 = suma 4 3
-- Regresa: 7

suma2 = suma (-9) (-1)
-- Regresa: (-10)

resta1 = resta 4 3
-- Regresa: 1

resta2 = resta 3 4
-- Regresa: 1

resta3 = resta 10 5
-- Regresa: 5

multiplica1 = multiplica 3 4
-- Regresa: 12

multiplica2 = multiplica 5 5
-- Regresa: 25

fact1 = fact 10
-- Regresa: 3628800

fact2 = fact 5
-- Regresa: 120

{-
Los primeros 20 números de fibonacci son los siguientes:
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

Pueden usarlos para comprobar cualquier número entre el primero (0) y
el último (20).
-}

ackerman1 = ackerman 1 2
-- Regresa: 4

ackerman2 = ackerman 3 4
-- Regresa: 125

{-
Para más ejemplos con la función de ackerman, pueden ver una tabla en
wikipedia.
-}

myFilter1 = myFilter isDigit "H0L4 C0M0 35TA5?"
-- Regresar: "0400355"

myFilter2 = myFilter isLower "HoLa CoMo EsTaS?"
-- Regresar: "oaoosa"

myFilter3 = myFilter isUpper "HoLa CoMo EsTaS?"
-- Regresar: "HLCMETS"

myFilter4 = myFilter isAlpha "H0L4 C0M0 35TA5?"
-- Regresar: "HLCMTA"

myMap1 = myMap toUpper "hola como estas?"
-- Regresar: "HOLA COMO ESTAS?"

myMap2 = myMap toLower "HOLA COMO ESTAS?"
-- Regresar: "hola como estas?"

myMap3 = myMap digitToInt ['0'..'9']
-- Regresar: [0,1,2,3,4,5,6,7,8,9]

l = [1..10]

myMap4 = map show l
-- Regresar: ["1","2","3","4","5","6","7","8","9","10"]

myMap5 = map read myMap4 :: [Int]
-- myMap5 = [1,2,3,4,5,6,7,8,9,10]
