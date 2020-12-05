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

module ListS where

data ListS a = NilS | Snoc (ListS a) a
instance (Show a)=> Show (ListS a)where 
  show (NilS)  = "< > "
  show (Snoc x a)  = "(" ++ show x   ++ ":: " ++ show a ++ ")" 

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | headS. Función que obtiene el primer elemento de la lista.
headS :: ListS a -> a
headS          NilS = error "D:"
headS (Snoc NilS a) = a
headS    (Snoc x a) = headS x 

-- | tailS. Función que obtiene la lista sin el primer elemento.
tailS :: ListS a -> ListS a
tailS          NilS = error "D:"
tailS (Snoc NilS a) = NilS
tailS    (Snoc x a) = (Snoc (tailS x)  a)  

-- | initS. Función que obtiene la lista sin el ultimo elemento.
-- lista.
initS :: ListS a -> ListS a
initS      NilS = NilS
initS (Snoc x a)= x

-- | lastS. Función que obtiene el ultimo elemento de la lista.
lastS :: ListS a -> a
lastS  NilS = error "D:"
lastS (Snoc x a) =  a

-- | lengthS. Función que obtiene la longitud de la lista.
lengthS :: ListS a -> Int
lengthS NilS = 0
lengthS (Snoc x a)= 1 + lengthS x

-- | nthElementS. Función que regresa el n-esimo elemento de la lista.
nthElementS :: Int -> ListS a -> a
nthElementS 0 x = headS x
nthElementS n x
  | n >= lengthS x || n < 0  = error "Invalid index"
  | otherwise = nthElementS (n-1)(tailS x)
  

-- | deleteNthElementS. Función que elimina el n-esimo elemento de la lista.
deleteNthElementS :: Int -> ListS a -> ListS a
deleteNthElementS 0 x = tailS x
deleteNthElementS n x
  | n >= lengthS x || n < 0  = error "Invalid index"
  | otherwise = addFirstS (headS x) (deleteNthElementS (n-1)(tailS x)) 

-- | addFirstS. Función que obtiene la lista donde el primer elemento es el elemento dado.
addFirstS :: a -> ListS a -> ListS a
addFirstS  a NilS = (Snoc NilS a)
addFirstS     a x = (Snoc (addFirstS a (initS x)) (lastS x) )

-- | addLastS. Función que obtiene a la lista donde el ultimo  elemento es el elemento dado.
addLastS :: a -> ListS a -> ListS a
addLastS a x = (Snoc x a)

-- | reverseS. Función que obtiene la reversa de la lista.
reverseS :: ListS a -> ListS a
reverseS  NilS = NilS
reverseS (Snoc NilS a) = (Snoc NilS a)
reverseS (Snoc x a) = addFirstS a (reverseS x)  

-- | appendS. Función que obtiene la concatenación de  dos listas.
appendS :: ListS a -> ListS a -> ListS a
appendS NilS NilS = NilS
appendS NilS x = x
appendS x NilS = x
appendS x x' = appendS (addLastS (headS x') x ) (tailS x')

-- | takeS. Función que obtiene la lista con los primeros n elementos.
-- listas.
takeS :: Int -> ListS a -> ListS a
takeS 0 x = (Snoc NilS (headS x))
takeS 1 x = (Snoc (Snoc NilS (headS x))(headS (tailS x)))
takeS n x
  | n > lengthS x = x 
  | otherwise = takeS (n-1) (initS x) 
  
-- | toHaskell. Función que pasa una función Snoc a una lista de Haskell.
toHaskell :: ListS a -> [a]
toHaskell NilS = []
toHaskell (Snoc x a) = toHaskell x ++ [a]

-- | fromHaskell. Función que pasa una función de Haskell a una lista Snoc. Puedes suponer que la lista que te pasan ya esta volteada.

fromHaskell :: [a] -> ListS a
fromHaskell [] = NilS
fromHaskell [x] = (Snoc NilS x)
fromHaskell (x:xs)=(Snoc (fromHaskell xs ) x)

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

-- Lista que contiene a los primeros cinco elementos.
l = Snoc (Snoc (Snoc (Snoc (Snoc  NilS 1) 2) 3) 4) 5

-- Lista que contiene a los elementos del 6-10.
m = Snoc (Snoc (Snoc (Snoc (Snoc  NilS 6) 7) 8) 9) 10

headS1 = headS l
-- Regresa: 1

headS2 = headS m
-- Regresa: 6

tailS1 = tailS l
-- Regresa: (((( < > :: 2) :: 3) :: 4) :: 5)

tailS2 = tailS m
-- Regresa: (((( < > :: 7) :: 8) :: 9) :: 10)

initS1 = initS l
-- Regresa: (((( < > :: 1) :: 2) :: 3) :: 4)

initS2 = initS m
-- Regresa: (((( < > :: 6) :: 7) :: 8) :: 9)

lastS1 = lastS l
-- Regresa: 5

lastS2 = lastS m
-- Regresa: 10

nthElementS1 = nthElementS 3 l
-- Regresa: 4

nthElementS2 = nthElementS 8 m
-- Regresa: ***Exception: Invalid index

deleteNthElementS1 = deleteNthElementS 5 l
-- Regresa: ***Exception: Invalid index

deleteNthElementS2 = deleteNthElementS 4 m
-- Regresa: (((( < > :: 6) :: 7) :: 8) :: 9)

addFirstS1 = addFirstS 0 l
-- Regresa: (((((( < > :: 0) :: 1) :: 2) :: 3) :: 4) :: 5)

addFirstS2 = addFirstS 5 m
-- Regresa: (((((( < > :: 5) :: 6) :: 7) :: 8) :: 9) :: 10)

addLastS1 = addLastS 6 l
-- Regresa: (((((( < > :: 1) :: 2) :: 3) :: 4) :: 5) :: 6)

addLastS2 = addLastS 11 m
-- Regresa: (((((( < > :: 6) :: 7) :: 8) :: 9) :: 10) :: 11)

reverseS1 = reverseS l
-- Regresa: (((((( < > :: 6) :: 7) :: 8) :: 9) :: 10) :: 11)

reverseS2 = reverseS m
-- Regresa: ((((( < > :: 10) :: 9) :: 8) :: 7) :: 6)

appendS1 = appendS l m
-- Regresa: (((((((((( < > :: 1) :: 2) :: 3) :: 4) :: 5) :: 
--6) :: 7) :: 8) :: 9) :: 10)

appendS2 = appendS m l
-- Regresa: (((((((((( < > :: 6) :: 7) :: 8) :: 9) :: 10) ::
--1) :: 2) :: 3) :: 4) :: 5)

takeS1 = takeS 10 l
-- Regresa: ((((( < > :: 1) :: 2) :: 3) :: 4) :: 5)

takeS2 = takeS 2 m
-- Regresa: (( < > :: 6) :: 7)

toHaskell1 = toHaskell l
-- Regresa: [1,2,3,4,5]

toHaskell2 = toHaskell m
-- Regresa: [6,7,8,9,10]

fromHaskell1 = fromHaskell $ reverse [1..3]
-- Regresa: ((( < > :: 1) :: 2) :: 3)

fromHaskell2 = fromHaskell $ reverse [6..9]
-- Regresa: (((( < > :: 6) :: 7) :: 8) :: 9)
